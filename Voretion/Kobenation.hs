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
import Data.Foldable
import Data.Tree.Zipper
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as S
import Voretion.Config

data Ineq a = a :<: a | FreeVal a

related :: (Eq a) => a -> [Ineq a] -> ([a], [a], [Ineq a])
related = go ([], [], [])
  where
    go x _ [] = x
    go (lt, mt, unrelated) x (a:t) =
      case a of
        FreeVal y | x == y -> go (lt, mt, unrelated) x t
                  | True   -> go (lt, mt, a:unrelated) x t
        y :<: z | x == y -> go (lt, z:mt, unrelated) x t
                | x == z -> go (y:lt, mt, unrelated) x t
                | True -> go (lt, mt, a:unrelated) x t

lessThan :: (Eq a)
         => a
         -> [Ineq a]
         -> ([a], [Ineq a])
lessThan = go ([], [])
  where
    go x _ [] = x
    go (lt, o) x (a:t) =
      case a of
        FreeVal y | x == y -> go (lt, o) x t
                  | True   -> go (lt, a:o) x t
        y :<: z | x == y -> go (lt, a:o) x t
                | x == z -> go (y:lt, o) x t
                | True -> go (lt, a:o) x t
        
-- | Try to sort elements according to a set of possibly nontransitive inequalities 
{-
The below algorithm is based on a lemma:
Set of relations Sₛ on set S is transitive ⇒ ∃ s∈S, {i:<:s|i∈S} = ∅
-}
trySort :: (Eq a)
        => [Ineq a]
        -> Maybe [a]
trySort τ = fmap (nub . reverse) $ go [] [] τ
  where
    go acc [] [] = Just acc
    go _   _  [] = Nothing
    go _   _  (a:<:b:_) | a == b = Nothing
    go acc acc2 (v@(FreeVal i):t) =
      let
        (lti, others) = lessThan i $ acc2 ++ t
      in
        case lti of
          [] -> go (i:acc) [] others
          _  -> go acc (v:acc2) t
    go acc acc2 (v@(i:<:j):t) =
      let
        (lti, others) = lessThan i $ acc2 ++ t
      in
        case lti of
          [] -> go (i:acc) [] $ FreeVal j:others
          _  -> go acc (v:acc2) t

findConnections :: LinkID
                -> Link
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
  (`evalStateT` 0) $ tvoretion cfg mate =<< pickRandom allRules
  -- (`evalStateT` 0) $ tvoretion cfg mate $ V.head allRules

tvoretion :: (MonadState Int m, MonadVoretion m)
          => Config
          -> (LinkID -> [Rule'])
          -> Maybe Int -- ^ Left boundary
          -> Maybe Int -- ^ Right boundary
          -> Rule'
          -> m [NLPWord]
tvoretion cfg mate b_l b_r seed = do
  myId <- get
  seedWord <- pickRandom $ _lval' seed {- TODO: Replace with simple random, we don't need to backtrack here! -}
  seedRule <- downhill cfg $ _links' seed
  let seedKob = KobNode myId $ Right (seedWord, seedRule)
  return []

type RuleZipper = TreePos Full NodeType

data KobNode = KobNode Int (Either LinkID (NLPWord, KobPair))
  deriving (Show)

instance Eq KobNode where
  (KobNode a _) == (KobNode b _) = a == b

type KobPair = ([KobNode], [KobNode])

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
  i <- get
  put $ i+1
  return 1

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
