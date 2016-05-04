{-
Yet another probabilistic monad
-}
module Control.Monad.Sample (
         Sample
       , choice
       , MonadSample(..)
       ) where

class Monad m => MonadSample m where
  fork :: Float -> a -> a -> m a
  guard :: Bool -> m ()

-- This type is used to specify the structure of computation
data Sample m a b =
  Fork {
    _metaInfo :: !m
  , _left
  , _right :: a -> Sample m a b
  , _bias :: !Float
  } |
  Val {
    _unVal :: b
  }

instance Functor (Sample m a) where
  fmap f a@Val{_unVal=v} = a{_unVal=f v}
  fmap f a@Fork{_left=l, _right=r} = a {
      _left = \a -> fmap f $ l a
    , _right = \a -> fmap f $ r a
    }

instance Applicative (Sample m a) where
  pure a = Val a

  Val{_unVal=f} <*> a = fmap f a
  f@Fork{_left=l, _right=r} <*> a = f {
      _left = \x -> (l x) <*> a
    , _right = \x -> (r x) <*> a
    }

choice :: MonadSample m => [(Float, a)] -> m a
choice l = undefined

{-

@
test a = do
  a <- fork (0.5, True) (0.5, False)
  if a then
    test $ a+1
  else
    return a
@

-}
