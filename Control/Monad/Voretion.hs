{-# LANGUAGE GADTs, RecordWildCards
  #-}
{-
Yet another probabilistic monad
-}
module Control.Monad.Voretion (
         Sample
       , choice
       , MonadVoretion(..)
       ) where

import Data.Function
import Data.List

class Monad m => MonadVoretion m where
  fork :: Float -> a -> a -> m a
  guard :: Bool -> m ()

-- | This type is used to keep the structure of computation,
-- it doesn't do anything on its own.
data Sample m a where
  Fork :: {
    _metaInfo :: !m
  , _left
  , _right :: r
  , _bias :: !Float
  , _next :: r -> Sample m a
  } -> Sample m a
  Val :: {
    _unVal :: a
  } -> Sample m a
  Zero :: Sample m a

instance Functor (Sample m) where
  fmap f a@Val{_unVal=v} = a{_unVal=f v}
  fmap f a@Fork{..} = Fork {
      _next = \a -> fmap f $ _next a
    , ..
    }

instance Applicative (Sample m) where
  pure a = Val a

  Val{_unVal=f} <*> a = fmap f a
  f@Fork{..} <*> a = Fork {
      _next = \x -> (_next x) <*> a
    , ..
    }

instance Monad (Sample m) where
  Val{_unVal=v} >>= f = f v
  Fork{..}      >>= f = Fork{ _next = \a -> _next a >>= f
                            , ..
                            }

class Default a where
  deFault :: a

instance Default () where
  deFault = ()

instance (Default m) => MonadVoretion (Sample m) where
  fork b x y = Fork {
      _metaInfo = deFault
    , _left = x
    , _right = y
    , _bias = b
    , _next = \a -> Val{_unVal=a}
    }

  guard True = return ()
  guard False = Zero

-- TODO: Not very effective, rewrite me
choice :: MonadVoretion m => [(Float, a)] -> m a
choice l = go (reverse $ scanl (\a b -> fst b + a) 0 l) $ reverse l
  where
    go _ [(_,x)] = return x
    go (pc:tp) ((p,x):t) = do
      c <- fork (p/pc) True False
      if c then
        return x
      else
        go tp t
        
noRandom :: Float -> Sample () b -> [(b, Float)]
noRandom epsilon = go 1
  where
    go _ Zero = []
    go n _ | n<epsilon = []
    go n Val{_unVal=v} = [(v, n)]
    go n Fork{_bias=b, _next=f, _left=l, _right=r} = go (n*b) (f l) ++ go (n*(1-b)) (f r)

-- histogram :: (Ord a) => Float -> Sample () b -> [(b, Float)]
-- histogram epsilon x = undefined -- TBD

{-

@
coin = fork 0.5 Tails Heads
@

@
test :: (MonadVoretion m) => Int -> m Int
test n = do
  a <- fork 0.5 False True
  if a then
    test $ n+1
  else
    return n
@

@
test2 :: (MonadVoretion m) => Int -> m Int
test2 n = sum <$> replicateM n (fork 0.5 0 1)
@

-}
