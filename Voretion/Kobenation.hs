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
          
findConnections :: LinkID
                -> Link
                -> [RuleZipper]
findConnections x = go S.empty . fromTree
  where go usedMacros z =
          case label z of
            Link {_link=l}
              | x =*= l -> [z]
              | True    -> []
            _ ->
              undefined
              -- (toList $ go usedMacros <$> firstChild z) ++ traverse usedMacros z

        traverse um x = go um <$> next x
              
natalyze :: (MonadVoretion m)
         => Config
         -> (LinkID -> [Rule'])
         -> Macros
         -> V.Vector Rule'
         -> m [NLPWord]
natalyze cfg mate macros allRules = (`evalStateT` 0) $ tvoretion cfg mate macros
                                      =<< deMacrify macros [] <$> pickRandom allRules

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
        
deMacrify :: Macros -> [MacroName] -> Rule' -> Rule'
deMacrify m l (Rule' ł r) =
    let
        f :: [MacroName] -> Link -> State [MacroName] Link
        f l₀ (Node α β) =
          case α of
            Macro n -> do
                if n `elem` l₀
                   then error $ "deMacrify: Loop detected, see macro " ++ n ++
                                " in the rule " ++ show ł
                   else return ()
                modify (n:) -- TODO: duplicates
                return $ m M.! n
            _ -> do
                c' <- mapM (f l₀) β
                return Node {
                             rootLabel = α
                           , subForest = c'
                           }

        (r', l₁) = (`runState` l) $ f l r
    in
      if r' == r
         then (Rule' ł r')
         else deMacrify m l₁ (Rule' ł r')
