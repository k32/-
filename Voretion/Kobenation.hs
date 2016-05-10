module Voretion.Kobenation (
    trySort
  ) where

import Control.Lens
import Control.Arrow
import Control.Monad.Voretion
import Control.Monad.Reader
import LinkGrammar.AST
import LinkGrammar.Process
import Voretion.Config
import Data.List
import Debug.Trace

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

lessThan :: (Eq a) => a -> [Ineq a] -> ([a], [Ineq a])
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
        
-- | Try to sort elements according to set of possibly nontransitive inequalities 
{-
The below algorithm is based on a lemma:
Set of relations Sₛ on set S is transitive ⇒ ∃ s∈S, {i:<:s|i∈S} = ∅
-}

trySort :: (Eq a) => [Ineq a] -> Maybe [a]
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
    
type Seed = ()

-- tvoretion :: (MonadVoretion m)
--           => Ruleset
--           -> Config
--           -> Seed
--           -> m [String]
-- tvoretion ruleset config wordSeed = undefined

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
