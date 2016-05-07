module Voretion.Kobenation (
       ) where

import Control.Lens
import Control.Arrow
import Control.Monad.Voretion
import LinkGrammar.AST
import LinkGrammar.Process
import Voretion.Config

-- Index of the _word_
type VarIdx = Int

type Seed = ()

tvoretion :: (MonadVoretion m) => Ruleset -> Config -> Seed -> m [String]
tvoretion ruleset config wordSeed = undefined

lvoretion :: (MonadVoretion m) => Config -> [Int] -> Link -> m ([LinkName], [LinkName])
lvoretion cfg idx (Node t c) =
  case t of
    Optional ->
      case idx of
        (_:idx') ->
          lvoretion idx' $ head c
        [] ->
          fork (_decay_optional) ([], []) =<< (lvoretion cfg [] $ head c)
    LinkAnd -> do
      let ρ (x, φ) = case idx of
                       (α:idx') | φ == α -> (idx', x)
                       _                 -> ([], x)
          l' = map ρ (zip c [0..])
      (concat *** concat) <$> unzip <$> mapM (uncurry $ lvoretion cfg) l'
    LinkOr ->
      case idx of
        [] -> do
          let n = length c
          i <- choice $ zip [0, n-1] [1..]
          lvoretion cfg [] $ c !! i
        (α:idx') ->
          lvoretion cfg idx' $ c !! α
    Link _ (LinkID n d)
      | null idx ->
          case d of
            Plus -> return ([], [n])
            Minus -> return ([n], [])
      | True ->
          return ([], [])
    MultiConnector -> do
      x <- fork (_decay_multi cfg) True False
      if x then
        
    Cost _ ->
      lvoretion cfg (drop 1 idx) $ head c
