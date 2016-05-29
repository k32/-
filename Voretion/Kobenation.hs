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
          
findConnections :: Macros
                -> LinkID
                -> Link
                -> [RuleZipper]
findConnections macros x = go S.empty . fromTree
  where go usedMacros z =
          case label z of
            Macro m
              | m `S.member` usedMacros ->
                  error $ "Macro loop detected: " ++ m
              | True ->
                  let
                    usedMacros' = (S.singleton m) `S.union` usedMacros
                    tree' = macros M.! m
                  in 
                    go usedMacros' $ setTree tree' z
            Link {_link=l}
              | x =*= l -> [z]
              | True    -> []
            _ ->
              (subForest $ tree z) >>= findConnections macros x
              
natalyze :: (MonadVoretion m)
         => Config
         -> (LinkID -> [Rule'])
         -> Macros
         -> V.Vector Rule'
         -> m [NLPWord]
natalyze cfg mate macros allRules = (`evalStateT` 0) $ tvoretion cfg mate macros =<< pickRandom allRules

tvoretion :: (MonadState Int m, MonadVoretion m)
          => Config
          -> (LinkID -> [Rule'])
          -> Macros
          -> Rule'
          -> m [NLPWord]
tvoretion cfg mate macros seed = do
  myId <- get
  seedWord <- pickRandom $ _lval' seed
  let rval = _links' seed
  rvalMate <- undefined
  -- print rvalMate
  -- neighbors <- mapM pickRandom $ findConnections macros rval rvalMate
  undefined

type RuleZipper = TreePos Full NodeType

data KobNode = KobNode Int LinkID NLPWord

flatten :: (MonadVoretion m, MonadState Int m)
        => Config
        -> RuleZipper
        -> m ([KobNode], [KobNode])
flatten cfg z = uphill ([], []) z
  where
    nextId :: (MonadState Int m) => m Int
    nextId = do
      i <- get
      put $ i+1
      return 1
      
    uphill t x =
      case label x of
        LinkOr{} ->
          case parent x of
            Just x' -> uphill t x'
            Nothing -> return t
        LinkAnd{} ->
          let
            left = foldl1 (++) $ map downhill $ before z
            right = foldl1 (++) $ map downhill $ after z
            both = (reverse (fst left) ++ fst right, snd left ++ snd right)
          in
            case parent x of
              Just x' -> uphill both x'
              Nothing -> both

    downhill l0@(Node label subforest) =
      case label of
        Optional _ -> do
          p <- fork (_decay_optional cfg) True False
          if p
             then downhill $ head subforest
             else return ([], [])
        MultiConnector _ -> do
          p <- fork (_decay_multi cfg) True False
          if p
             then (++) *** (++) $ (downhill $ head subforest) (downhill l0) -- TODO: optimize
             else return ([], []) 
        

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
