{-# LANGUAGE FlexibleContexts #-}
module Voretion.Kobenation (
    trySort
  , natalyze
  ) where

import Control.Lens
import Control.Arrow
import Control.Monad.Voretion
import Control.Monad.Reader
import Control.Monad.State
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

Now we can translate it to set of inequalities:
0: [2:<:0, 0:<:1]
1: [0:<:1]
2: [3:<:2, 2:<:0]
...

And simply concatenate the system:
[2:<:0, 0:<:1, 0:<:1, 3:<:2, 2:<:0]


Then try sorting it:
[3,2,0,1]

Backtrack if the system isn't feasible, otherwise resolve remaining rows.

Stop if there is no unresolved rules.

* Example of invalid grammar:

%      _____
%     /____ \
%   _/____ \ \
% /  |    \| |
% 3  0     1 2


0: [*0, 1, 2]      % 0 : A+ & B+;
1: [3, 0, *1]      % 1 : C- & A-;
2: [0, *2]         % 2 : B-;
3: [3, 1]          % 3 : C+;

Ineqs: [[0:<:1, 1:<:2], [3:<:0, 0:<:1], [0:<:2], [3:<:1]]

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

data State =
  State {
    _currentId 
  , _lastResolvedId :: Int
  }

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
natalyze cfg mate allRules = undefined


tvoretion :: (MonadState State m, MonadVoretion m)
          => Config
          -> (LinkID -> [Rule'])
          -> Kobenation
          -> m Kobenation
tvoretion cfg mate kob = 

order :: [a]
      -> [Ineq a]
order l = map (uncurry (:<:)) $ zip l $ drop 1 l

tvoretion' :: (MonadState Int m, MonadVoretion m)
           => Config
           -> (LinkID -> [Rule'])
           -> KobNode
           -> m KobNode
tvoretion' cfg mate (KobNode myId (Left link)) = do
  r <- pickRandom $ mate link
  w <- pickRandom $ _lval' r {- TODO: again, we don't need to backtrack here -}
  kp <- kobenate cfg =<< pickRandom (findConnections link $ _links' r)
  return $ KobNode myId $ Right (w, kp)

instance Eq KobNode where
  (KobNode a _) == (KobNode b _) = a == b

(\++/) :: KobPair -> KobPair -> KobPair
(a1, b1) \++/ (a2, b2) = (a1 ++ a2, b1 ++ b2)  

kobenate :: (MonadVoretion m, MonadState Int m)
         => Config
         -> RuleZipper
         -> m KobPair
kobenate cfg z = uphill ([], []) z
  where
    climb t x =
      case parent x of
        Just x' -> uphill t x'
        Nothing -> return t

    uphill t x =
      case label x of
        MultiConnector{} -> do
          m <- downhill cfg (tree x)
          let t' = t \++/ m
          climb t' x 
        LinkOr{} ->
          climb t x
        _ -> do
          i <- downhill' cfg $ before z
          j <- downhill' cfg $ after z
          climb (i \++/ t \++/ j) x

nextId :: (MonadState Int m) => m Int
nextId = do
  s@State{_currentId=i} <- get
  put $ i{_currentId=i+1}
  return i

downhill' :: (MonadState Int m, MonadVoretion m)
          => Config
          -> [Link]
          -> m KobPair
downhill' cfg x = foldl (\++/) ([], []) <$> mapM (downhill cfg) x

downhill :: (MonadState Int m, MonadVoretion m)
         => Config
         -> Link
         -> m KobPair
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
      x <- nextId
      let ret = [KobNode x (Left i)]
      case _linkDirection i of
        Plus  -> return ([], ret)
        Minus -> return (ret, [])
