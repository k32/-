{-# LANGUAGE DeriveGeneric #-}
module Data.TernaryTree
       (
         TTree
       , relaxedLookup
       , empty
       , (!)
       , insertWith
       , insert
       , fromList
       , toList
       , foldWithKey
       ) where

import qualified Data.Foldable as Fld
import Data.Traversable
import Data.Maybe
import GHC.Generics
import Data.Binary
import Debug.Trace

data TTree k v =
  TNode {
    _key :: !k
  , _val :: !(Maybe v)
  , _eq :: !(TTree k v)
  , _left :: !(TTree k v)
  , _right :: !(TTree k v)
  , _height :: !Int
  }
  | TTNil
  deriving (Show, Generic)
instance (Binary k, Binary v) => Binary (TTree k v)

-- With no balancing whatsoever by now...
insert :: (Ord k)
       => [k]               -- ^ Key
       -> v                 -- ^ Value
       -> TTree k v         -- ^ Tree
       -> TTree k v
insert = insertWith (\_ b -> b)
{-# SPECIALIZE insert :: [Char] 
                      -> v
                      -> TTree Char v
                      -> TTree Char v
  #-}

insertWith :: (Ord k)
           => (v -> v -> v) -- ^ Conflict resolution function
           -> [k]           -- ^ Key
           -> v             -- ^ Value
           -> TTree k v     -- ^ Tree
           -> TTree k v
insertWith _ [] = error "FuzzyMap.hs: attempt to insert object with empty key"
insertWith f k = insertWith' f k (length k)
{-# SPECIALIZE insertWith :: (v -> v -> v) 
                          -> [Char] 
                          -> v 
                          -> TTree Char v
                          -> TTree Char v
  #-}

insertWith' :: (Ord k)
            => (v -> v -> v) -- ^ Conflict resolution function
            -> [k]           -- ^ Key
            -> Int           -- ^ Length of the key
            -> v             -- ^ Value
            -> TTree k v     -- ^ Tree
            -> TTree k v
insertWith' f k1@(k:kt) h v t = 
  case t of
    TTNil ->
      insertWith' f k1 h v $ TNode {
          _key = k
        , _eq = TTNil
        , _left = TTNil
        , _right = TTNil
        , _val = Nothing
        , _height = h
        }
    node@TNode{_key=k0, _height=h0, _val=v0, _eq=eq0, _left=left0, _right=right0} ->
      case compare k0 k of
        EQ | null kt ->
               node {
                 _val = Just $ maybe v (flip f $ v) v0
               }
           | True ->
               node {
                 _eq = insertWith' f kt (h-1) v eq0
               , _height = max h h0
               }
        GT ->
           node {
             _left = insertWith' f k1 h v left0
           , _height = max h h0
           }
        LT ->
           node {
             _right = insertWith' f k1 h v right0
           , _height = max h h0
           }
{-# SPECIALIZE insertWith' :: (v -> v -> v) 
                           -> [Char] 
                           -> Int
                           -> v 
                           -> TTree Char v
                           -> TTree Char v
  #-}

relaxedLookup :: (k -> k -> Ordering) -- ^ Comparison function
              -> Bool                 -- ^ Include prefix matches
              -> TTree k v            -- ^ Tree
              -> [k]                  -- ^ Key
              -> [([k], v)]
relaxedLookup prefix f = relaxedLookup' prefix f []

relaxedLookup' :: (k -> k -> Ordering) -- ^ Comparison function
               -> Bool                 -- ^ Include prefix matches
               -> [k]                  -- ^ Trail
               -> TTree k v            -- ^ Tree
               -> [k]                  -- ^ Key
               -> [([k], v)]
relaxedLookup' _ _ _ TTNil _ = []
relaxedLookup' _ False _ _ [] = []
relaxedLookup' _ True trail n [] = map (\(k, v) -> (reverse trail ++ k, v)) $ toList n
relaxedLookup' f prefix trail TNode{_key=k0, _val=v0, _left=l0, _right=r0, _eq=e0} ko@(k1:kt) =
  let
    trail' = k1:trail
    
    mkpair (Just x)
      | prefix || null kt = [(reverse trail', x)]
    mkpair _ = []
    {-# INLINE mkpair #-}
  in case f k0 k1 of
       EQ ->
         mkpair v0 ++
         relaxedLookup' f prefix trail' e0 kt ++
         relaxedLookup' f prefix trail' l0 ko ++
         relaxedLookup' f prefix trail' r0 ko
       GT ->
         relaxedLookup' f prefix trail l0 ko
       LT ->
         relaxedLookup' f prefix trail r0 ko

(!) :: (Ord k) => TTree k v -> [k] -> Maybe v
t ! k = fmap snd $ listToMaybe $ relaxedLookup compare False t k
{-# SPECIALIZE (!) :: TTree Char v -> String -> Maybe v #-}


fromList :: (Ord k)
         => [([k], v)]
         -> TTree k v
fromList =  foldl (flip $ uncurry insert) empty
{-# SPECIALIZE fromList :: [(String, v)] -> TTree Char v #-}

toList :: TTree k v
       -> [([k], v)]
toList = foldWithKey (\k v a -> (k, v):a) []

foldWithKey :: ([k] -> v -> a -> a)
            -> a
            -> TTree k v
            -> a
foldWithKey f a t = foldWithKey' [] f t a

foldWithKey' :: [k]
             -> ([k] -> v -> a -> a)
             -> TTree k v
             -> a
             -> a
foldWithKey' _ _ TTNil a = a
foldWithKey' trail f TNode{_val=v, _eq=e, _left=l, _right=r, _key=k} a =
  let
    trail' = k:trail
    this = case v of
             Just x -> f (reverse trail') x
             Nothing -> id
  in
    foldWithKey' trail f r $ foldWithKey' trail' f e $ this $ foldWithKey' trail f l a

instance Functor (TTree k) where
  fmap _ TTNil = TTNil
  fmap f n@TNode{_val=v, _eq=e, _left=l, _right=r} =
    n {
        _val = fmap f v
      , _eq  = fmap f e
      , _left = fmap f l
      , _right = fmap f r
      }

empty = TTNil
