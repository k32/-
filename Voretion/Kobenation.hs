{-# LANGUAGE FlexibleContexts #-}
module Voretion.Kobenation (
    trySort
  , natalyze
  ) where

import Control.Lens
import Control.Arrow
import Control.Monad.Voretion
import Control.Monad.State hiding (guard)
import LinkGrammar.AST
import LinkGrammar.Process
import Voretion.Config
import Data.List
import Debug.Trace
import Data.PrettyPrint
import Data.Foldable
import Data.Tree.Zipper
import Data.Nontransitive
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as S
import Voretion.Config
import Data.Maybe (mapMaybe)

{-
* Overview of the Topological Voretion Method:

Kobenation is:

    +-- Vector of NLP Words and "pointers"
    |
    |          +-- Star emphasizes position     +-- What original (simplified)
    |          |   of the current NLP word      |   rule looks like
    v          v                                v
0: |W₁| => [2, *0, 1]  \                     %  W₁ : A- & B+
1: |W₂| => [0, *1]      > Resolved rules     %  W₂ : B-
2: |W₃| => [3, *2, 0]  /                     %  W₃ : C- & A+
3: [C-]       <--- Remaining rules aren't resolved yet
....

Elements of the vector are called called Kobs

Now we can translate Kobenation to a set of 1st order constraints:
0: [2:<:0, 0:<:1]
1: [0:<:1]
2: [3:<:2, 2:<:0]
...

2nd order constraints are needed to prevent crossing links like on the
below picture:

%      _____
%     /____ \
%   _/____ \ \
% /  |    \| |
% 3  0     1 2

0: [*0, 1, 2]      % 0 : A+ & B+;
1: [3, 0, *1]      % 1 : C- & A-;
2: [0, *2]         % 2 : B-;
3: [3, 1]          % 3 : C+;

1st order ineqs alone are not sufficient to prevent link intersection:
[[0:<:1, 1:<:2], [3:<:0, 0:<:1], [0:<:2], [3:<:1]] are sortable:
[3, 0, 1, 2]

2nd order constraints are found according to the following algorithm:
let A₀ is an element of a kobenation K. For all pointers in A₀ except
the starred one, lookup the row in K, say A_n, take the first and the
last elements from A_n, then make sure last(A_n) :<: first(A_{n+1})

And simply concatenate all 1st and 2nd order constraints:
[2:<:0, 0:<:1, 0:<:1, 3:<:2, 2:<:0, ...]


Backtrack if the system isn't feasible, otherwise resolve remaining
rows.

Stop if there are no unresolved rules.

-}


type Pointer = Int

type RuleZipper = TreePos Full NodeType

data Kob =
  Resolved {
    _word :: NLPWord
  , _order :: [Pointer]
  } |
  Unresolved LinkID
  deriving (Show)

type Kobenation = V.Vector Kob

data MyState =
  State {
    _currentId 
  , _lastResolvedId :: Int
  }

type KobZipper = ([LinkID], [LinkID])

{- | Given a rule, return list of zippers pointing to links mating with
the given one -}
findConnections :: LinkID -- ^ LinkID to match with
                -> Link   -- ^ Rule
                -> [RuleZipper]
findConnections x = go . fromTree
  where go z =
          case label z of
            Link {_link=l}
              | x' =*= l -> [z]
              | True     -> []
            _ ->
              go' (firstChild z) ++ go' (next z)

        go' x = (toList x) >>= go

        x' = flipLink x
        
natalyze :: (MonadVoretion m)
         => Config
         -> (LinkID -> [Rule'])
         -> V.Vector Rule'
         -> m [NLPWord]
natalyze cfg mate allRules =
  let
    state₀ = State {
               _currentId = 0
             , _lastResolvedId = 0
             }

    go :: (MonadVoretion m, MonadState MyState m)
       => Kobenation
       -> m (Kobenation, [Pointer])
    go kob = do
      State{_lastResolvedId=lastId, _currentId=currId} <- get
      sorted <- liftMaybe $ trySort $ getConstraints kob
      if currId - 1 == lastId
         then return (kob, sorted)
         else do
           go =<< tvoretion cfg mate kob

    dbg kob x | _debug cfg = trace (unlines $ map f $ V.toList kob) $ return x
              | True       = return x
       where f Resolved{_word=NLPWord{_nlpword=w}, _order=o} = "KOBENATION: " ++ w ++ " " ++ show o
             
  in (`evalStateT` state₀) $ do
    Rule' {_lval'=words, _links'=seed} <- pickRandom allRules
    word <- pickRandom words {- TODO: We don't need to backtrack here! #-}
    myId <- nextId
    kob₀ <- addRows (V.singleton undefined) (myId, word) =<< downhill cfg seed
    (kob', order) <- go kob₀
    guard $ not $ null order
    dbg kob' $ map (_word . (kob' V.!)) order


getConstraints :: Kobenation
               -> [Ineq Pointer]
getConstraints kob =
  let
    resolved (Resolved _ x) = Just x
    resolved _ = Nothing

    resolved' (Resolved _ x) = x
    resolved' _ = error "resolved': Not resolved"
    
    resRows = mapMaybe resolved $ V.toList kob

    order1st l = map (uncurry (:<:)) $ zip l $ drop 1 l

    order2nd i = map (\(a, b) -> Data.List.last a :<: head b) $ zip ll $ drop 1 ll
      where ll = mapMaybe f $ resolved' $ kob V.! i
            
            f j | i /= j = resolved $ kob V.! j
                | True   = Just [i]
    ret = (resRows >>= order1st) ++ ([0..length resRows - 1] >>= order2nd)
  in
    {- trace (show $ ret) $ -} ret

addRows :: (MonadState MyState m)
        => Kobenation
        -> (Pointer, NLPWord)
        -> KobZipper
        -> m Kobenation
addRows k₀ (up, w) (before, after) = do {- TODO: Check +/- order accordingly -}
  before' <- replicateM (length before) nextId
  after' <- replicateM (length after) nextId
  let row = before' ++ (up : after')
      new = V.fromList $ map Unresolved $ before ++ after
  state <- get
  put $ state{_lastResolvedId=up}
  return $ (k₀ V.// [(up, Resolved w row)]) V.++ new
  
tvoretion :: (MonadState MyState m, MonadVoretion m)
           => Config
           -> (LinkID -> [Rule'])
           -> Kobenation
           -> m Kobenation
tvoretion cfg mate kob₀ = do
  i <- (+1) <$> _lastResolvedId <$> get
  let Unresolved link = kob₀ V.! i
  r <- pickRandom $ mate link
  w <- pickRandom $ _lval' r {- TODO: again, we don't need to backtrack here -}
  row <- kobenate cfg =<< pickRandom (findConnections link $ _links' r)
  addRows kob₀ (i, w) row

(\++/) :: KobZipper -> KobZipper -> KobZipper
(a1, b1) \++/ (a2, b2) = (a1 ++ a2, b1 ++ b2)  

kobenate :: (MonadVoretion m)
         => Config
         -> RuleZipper
         -> m KobZipper
kobenate cfg z = uphill ([], []) z 
  where
    climb t x =
      case parent x of
        Just x' -> uphill t x'
        Nothing -> return t

    uphill t x = (if _debug cfg
                    then trace ("UPHILL: (" ++ show t ++ ") "  ++ show (label x))
                    else id) $
      case label <$> parent x of
        Just (LinkAnd{}) -> do
          i <- downhill' cfg $ before x
          j <- downhill' cfg $ after x
          climb (i \++/ t \++/ j) x
        _ ->
          case label x of
            MultiConnector{} -> do
              m <- downhill cfg (tree x)
              let t' = t \++/ m
              climb t' x
            _ ->
              climb t x

nextId :: (MonadState MyState m)
       => m Int
nextId = do
  s@State{_currentId=i} <- get
  put $ s{_currentId=i+1}
  return i

downhill' :: (MonadVoretion m)
          => Config
          -> [Link]
          -> m KobZipper
downhill' cfg x = foldl (\++/) ([], []) <$> mapM (downhill cfg) x

downhill :: (MonadVoretion m)
         => Config
         -> Link
         -> m KobZipper
downhill cfg l0@(Node label subforest) =
  case label of
    Optional _ ->
      ifR (_decay_optional cfg)
         {-then-} (downhill cfg $ head subforest)
         {-else-} (return ([], []))
    MultiConnector _ ->
      ifR (_decay_multi cfg)
         {-then-} (do
           a <- downhill cfg l0
           b <- downhill cfg $ head subforest
           return $ a \++/ b)
         {-else-} (return ([], []))
    LinkOr{} ->
      downhill cfg =<< pickRandom subforest
    LinkAnd{} ->
      downhill' cfg subforest
    Cost{} ->
      downhill' cfg subforest
    EmptyLink ->
      return ([], [])
    Link{_link=i} -> do
      case _linkDirection i of
        Plus  -> return ([], [i])
        Minus -> return ([i], [])
