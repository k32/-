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
import Data.Either (lefts)

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

An elements of the vector is called called Kob

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
  Unresolved Pointer LinkID
  deriving (Show)

type Kobenation = V.Vector Kob

data MyState =
  State {
    _currentId 
  , _lastResolvedId :: Int
  }

type QuasiLink = Either LinkID Pointer

type Disjunct = ([QuasiLink], [QuasiLink])

type DisjunctZ = (([QuasiLink], [QuasiLink], Maybe QuasiLink)
                 ,([QuasiLink], [QuasiLink], Maybe QuasiLink))

-- Add a + link left of the current one
(+|) :: DisjunctZ -> QuasiLink -> DisjunctZ
d +| l = d&_2._1%~(l:)

-- Add a + link right of the current one
(|+) :: DisjunctZ -> QuasiLink -> DisjunctZ
d |+ l = d&_2._2%~(l:)

-- Add a - link left of the current one
(-|) :: DisjunctZ -> QuasiLink -> DisjunctZ
d -| l = d&_1._1%~(l:)

-- Add a - link right of the current one
(|-) :: DisjunctZ -> QuasiLink -> DisjunctZ
d |- l = d&_1._2%~(l:)

toDisjunct :: DisjunctZ -> Disjunct
toDisjunct ((a,b,x), (c,d,y)) = (reverse a ++ toList x ++ b, reverse c ++ toList y ++ d)

{- | Given a rule, return list of zippers pointing to links mating with
the given one -}
findConnections :: LinkID -- ^ LinkID to match with
                -> Link   -- ^ Rule
                -> [RuleZipper]
findConnections x = go . fromTree
  where go z =
          case label z of
            Link {_link=l}
              | x' =*= l -> z : (go' $ next z)
              | True     -> go' $ next z
            _ ->
              go' (firstChild z) ++ go' (next z)

        go' x = (toList x) >>= go

        x' = flipLink x

dbgKob :: Config
       -> Kobenation
       -> a
       -> a
dbgKob cfg kob x | _debug cfg = trace (unlines $ map f $ V.toList kob) x
                 | True       = x
   where f Resolved{_word=NLPWord{_nlpword=w}, _order=o} = "KOBENATION: " ++ w ++ " " ++ show o
         f (Unresolved link p) = "KOBENATION: " ++ show link ++ " " ++ show p


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
      sorted <- liftMaybeCry (_debug cfg) "FAIL: Order failure" $
                  dbgKob cfg kob $ trySort $ getConstraints cfg kob
      if currId - 1 == lastId
         then return (kob, sorted)
         else do
           go =<< tvoretion cfg mate kob

  in (`evalStateT` state₀) $ do
    Rule' {_lval'=words, _links'=seed} <- pickRandom True allRules
    word <- pickRandom False words
    myId <- nextId
    kob₀ <- addRows (V.singleton undefined) (myId, word) =<< downhill cfg seed
    (kob', order) <- go kob₀
    guardCry (_debug cfg) "FAIL: Empty order" $ not $ null order
    return $ dbgKob cfg kob' $ map (_word . (kob' V.!)) order


getConstraints :: Config
               -> Kobenation
               -> [Ineq Pointer]
getConstraints cfg kob =
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

    constr1st = (resRows >>= order1st)
    constr2nd = ([0..length resRows - 1] >>= order2nd)

    dbg | _debug cfg =
           trace ("1st order constraints: " ++ show constr1st ++
                  "\n2nd order constraints: " ++ show constr2nd)
        | True = id
  in
    dbg $ constr1st -- ++ constr2nd

addRows :: (MonadState MyState m)
        => Kobenation
        -> (Pointer, NLPWord)
        -> Disjunct
        -> m Kobenation
addRows k₀ (up, w) (before, after) =
  let
    go (Left _) = nextId
    go (Right x) = return x
  in do {- TODO: Check +/- order accordingly -}
    before' <- mapM go before
    after' <- mapM go after
    let row = before' ++ (up : after')
        new = V.fromList $ map (Unresolved up) $ lefts $ before ++ after
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
  let Unresolved myId link = kob₀ V.! i
  r <- pickRandom True $ mate link
  w <- pickRandom False $ _lval' r
  let conns = findConnections link $ _links' r
      d₀ = case _linkDirection link of
             Minus -> ([], [Right myId])
             Plus  -> ([Right myId], [])
  row <- kobenate cfg d₀ =<< pickRandom True conns
  addRows kob₀ (i, w) row

(\++/) :: Disjunct -> Disjunct -> Disjunct
(a1, b1) \++/ (a2, b2) = (a1 ++ a2, b1 ++ b2)  

kobenate :: (MonadVoretion m)
         => Config
         -> Disjunct
         -> RuleZipper
         -> m Disjunct
kobenate cfg d₀ z = uphill d₀ z 
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
          i <- downhill' cfg $ reverse $ before x
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
          -> m Disjunct
downhill' cfg x = foldl (\++/) ([], []) <$> mapM (downhill cfg) x

downhill :: (MonadVoretion m)
         => Config
         -> Link
         -> m Disjunct
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
      downhill cfg =<< pickRandom True subforest
    LinkAnd{} ->
      downhill' cfg subforest
    Cost{} ->
      downhill' cfg subforest
    EmptyLink ->
      return ([], [])
    Link{_link=i} -> do
      case _linkDirection i of
        Plus  -> return ([], [Left i])
        Minus -> return ([Left i], [])
