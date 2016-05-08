module Voretion.Kobenation (
    insertRow 
  ) where

import Control.Lens
import Control.Arrow
import Control.Monad.Voretion
import LinkGrammar.AST
import LinkGrammar.Process
import Voretion.Config
import qualified Data.Vector as V
import Math.LinearAlgebra.Sparse.Matrix

type Seed = ()

-- | Sentence matrix
type Σ = () -- TBD

-- | Permutation vector
type Π a = V.Vector a

insertRow :: Σ    -- ^ Order matrix
          -> Π a  -- ^ Old permutation vector
          -> Σ    -- ^ 1-row matrix with inequalities of a new element
          -> a    -- ^ The new element
          -> Maybe (Σ, Π a)
insertRow a₀ v₀ r = undefined

tvoretion :: (MonadVoretion m)
          => Ruleset
          -> Config
          -> Seed
          -> m [String]
tvoretion ruleset config wordSeed = undefined

lvoretion :: (MonadVoretion m, MonadReader (Σ, Π Φ))
          => Config
          -> [Int]
          -> Link
          -> m ([LinkName], [LinkName])
lvoretion cfg{_cost_cost=ξ, _decay_optional=σ₁, _decay_multi=σ₂} idx (Node t c) =
  case t of
    Optional cost ->
      case idx of
        (_:idx') ->
          -- idx /= [] means that certain element should be taken instead of a random one
          lvoretion idx' $ head c
        [] -> do
          let p = max σ₁ (ξ * cost)
          right <- lvoretion cfg [] $ head c
          fork p ([], []) right
    LinkAnd _ -> do
      let ρ (x, φ) = case idx of
                       (α:idx') | φ == α -> (idx', x)
                       _ -> ([], x)
          l' = map ρ (zip c [0..])
      (concat *** concat) <$> unzip <$> mapM (uncurry $ lvoretion cfg) l'
    LinkOr _ ->
      case idx of
        [] -> do
          c <- choice $ zip c $ map (\x -> 1/(1+getCost x)) c
          lvoretion cfg [] c
        (α:idx') ->
          lvoretion cfg idx' $ c !! α
    Link _ (LinkID n d)
      | null idx ->
          case d of
            Plus -> return ([], [n])
            Minus -> return ([n], [])
      | True ->
          return ([], [])
    MultiConnector cost -> do
      let 
