module Data.Nontransitive (
    Ineq(..)
  , related
  , trySort
  , tryGroup
  ) where

import Data.List (nub)
import qualified Data.Vector as V
import Data.Graph
import Control.Monad.ST

data Ineq a = a :<: a | FreeVal a
  deriving (Show, Eq)

data Compare = Less | More | Unknown

data IneqGraph = IeqGraph {
      _vtx2subgraph :: V.Vector Int
    , _subgraphs :: V.Vector [[Int]]
    }

-- analyze :: [Ineq Int] -> IneqGraph
-- analyze ineq =
--     let
--         lval (a :<: _) = a
--         rval (_ :<: a) = a
--         l = 1 + maximum $ map (\(a :<: b) -> max a b) ineq
--         ineq_graph = [(i, i, [j | (j:<:k) <- ineq, k==i])| i<-[0..l-1]]
--         scc = stronglyConnComp ineq_graph
--     in runST $ do
--       color <- V.generateM l (const -1)
--       forM_ (zip [0..] scc) $ \(c, i) ->
--           forM_ i $ \j ->
--               color V.
  
           
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

-- | Returns list of elements lesser than a given one, according to a set of
-- inequalities and list of inequalities unrelated to this element
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

tryGroup :: (Eq a)
         => [Ineq a]
         -> Maybe [[a]]
tryGroup =
  let
    supp [] _ = True
    supp (FreeVal _:t) i = supp t i
    supp (a:<:_:t) i | a == i = False
                     | True   = supp t i
      
    sup ineq = do
      i <- ineq
      let i' = case i of
                 FreeVal a -> a
                 (_ :<: a) -> a
      case supp ineq i' of
        True  -> return i'
        False -> []

    filterOut l x@(FreeVal a) = if (a==) `any` l then [] else return x
    filterOut l x@(b:<:a)     = return $ if (a==) `any` l then FreeVal b else x

    go acc [] = Just acc
    go acc remaining =
        case sup remaining of
          [] -> Nothing
          l  -> go (l':acc) $ filterOut l' =<< remaining
              where l' = nub l
  in
    go []
