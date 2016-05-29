{-# LANGUAGE FlexibleContexts #-}
module Voretion.Kobenation (
    trySort
  , kobenate
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
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable

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
    -- uniqInsert a l
    --   | a `elem` l = l
    --   | True       = (a:l)
    
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

linkFoldlPath :: (MonadReader [Int] m, Monad m)
              => Macros
              -> (a -> NodeType -> m a)
              -> a
              -> Link
              -> m a
linkFoldlPath macros f a0 l0 = (`runReaderT` []) $ go S.empty a0 l0
  where go usedMacros a Node{rootLabel=label, subForest=sf} =
          case label of
            Macro m -> do
              when (m `S.member` usedMacros) $
                error $ "Macro loop detected: " ++ m ++ " in link: " ++ show l0
              let usedMacros' = (S.singleton m) `S.union` usedMacros
              go usedMacros' a $ macros M.! m
            _ ->
              go usedMacros (f a label)
          
findConnections :: Macros
                -> LinkID
                -> Link
                -> [[Int]]
findConnections macros x l0 = undefined -- (`runReader` (S.empty, [])) $ go l0
  -- where go (Node Link{_link = l} _)
  --         | l =*= x = return [views _2]
  --         | True    = return []
  --       go (Node (Macro m) _) = do
  --         mUsed <- views _1
  --         when (m `S.member` mUsed) $
  --            error $ "Macro loop detected: " ++ m
  --         _1 %= (S.union (S.singleton m))
  --         go $ macros M.! m
          

kobenate :: (MonadVoretion m)
         => (LinkID -> [Rule'])
         -> Macros
         -> V.Vector Rule'
         -> m [NLPWord]
kobenate mate macros allRules = tvoretion 0 mate macros =<< pickRandom allRules

tvoretion :: (MonadVoretion m)
          => Int
          -> (LinkID -> [Rule'])
          -> Macros
          -> Rule'
          -> m [NLPWord]
tvoretion myIdx mate macros seed = do
  seedWord <- pickRandom $ _lval' seed
  let rval = _links' seed
  -- rvalMate <- pickRandom $ mate rval
  -- neighbors <- mapM pickRandom $ findConnections macros rval rvalMate
  undefined
  

-- lvoretion :: (MonadVoretion m, MonadReader (Σ, Π Φ))
--           => Config
--           -> [Int]
--           -> Link
--           -> m ([LinkName], [LinkName])
-- lvoretion cfg{_cost_cost=ξ, _decay_optional=λ₁, _decay_multi=λ₂} idx (Node t c) =
--   case t of
--     Optional cost ->
--       case idx of
--         (_:idx') ->
--           -- idx /= [] means that certain element should be taken instead of a random one
--           lvoretion idx' $ head c
--         [] -> do
--           let p = max σ₁ (ξ * cost)
--           right <- lvoretion cfg [] $ head c
--           fork p ([], []) right
--           (σ, π) <- ask
--           liftMaybe $ insertRow σ 
--     LinkAnd _ -> do
--       let ρ (x, φ) = case idx of
--                        (α:idx') | φ == α -> (idx', x)
--                        _ -> ([], x)
--           l' = map ρ (zip c [0..])
--       (concat *** concat) <$> unzip <$> mapM (uncurry $ lvoretion cfg) l'
--     LinkOr _ ->
--       case idx of
--         [] -> do
--           c <- choice $ zip c $ map (\x -> 1/(1+getCost x)) c
--           lvoretion cfg [] c
--         (α:idx') ->
--           lvoretion cfg idx' $ c !! α
--     Link _ (LinkID n d)
--       | null idx ->
--           case d of
--             Plus -> return ([], [n])
--             Minus -> return ([n], [])
--       | True ->
--           return ([], [])
--     MultiConnector cost -> do
--       let 
