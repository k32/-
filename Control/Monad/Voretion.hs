{-# LANGUAGE GADTs, RecordWildCards, FlexibleInstances
  #-}
{-
Yet another probabilistic monad.

Disclaimer: by no means it's intended for serious
applications. Correctness and performance weren't tested nor guaranteed!
-}
module Control.Monad.Voretion (
         Sample
       , choice
       , liftMaybe
       , MonadVoretion(..)
       , PickRandom(..)
       ) where

import Data.Function
import Data.List
import System.Random
import qualified Data.Map as M
import Control.Monad.Cont hiding (guard)

class Monad m => MonadVoretion m where
  -- | Bernoulli distribution. Returns either of the arguments with
  -- certain probability
  fork :: Float  -- ^ Probability of the first value
       -> a      -- ^ First value
       -> a      -- ^ Second value
       -> m a

  -- | Aborts execution if some condition isn't met
  guard :: Bool  -- ^ Condition
        -> m ()

  -- | Returns a random value from a range
  getRandomR :: (Random a)
             => (a, a)  -- ^ Range 
             -> m a

  -- TODO: Not very effective, rewrite me
  -- ...Also there's summation of many floats, I smell problems with that
  choice :: MonadVoretion m
         => [(Float, a)]
         -> m a
  choice l = go (reverse $ scanl (\a b -> fst b + a) 0 l) $ reverse l 
    where
      go _ [(_,x)] = return x
      go (pc:tp) ((p,x):t) = do
        c <- fork (p/pc) True False
        if c then
          return x
        else
          go tp t


-- | This type is used to keep the structure of computation, it
-- doesn't do anything on its own.  One needs an "voretion engine" to
-- evaluate this.  Different evaluation strategies are possible.
data Sample m a where
  -- | Fork picks either value from a pair. The choice is random and
  -- _bias determines probability of the first value.
  Fork :: {
    _metaInfo :: !m           -- ^ Opaque data, it may be used by the execution engine
  , _left                     -- ^ First value
  , _right :: r               -- ^ Second value
  , _bias :: !Float           -- ^ Bias towards the first value
  , _next :: r -> Sample m a  -- ^ Next expression
  } -> Sample m a
  -- | Val is just a pure value
  Val :: {
    _unVal :: a
  } -> Sample m a
  -- | Guard checks some expression and backtracks if it isn't true
  Guard :: {
    _metaInfo :: !m           -- ^ Opaque data, it may be used by the execution engine
  , _guarded :: Sample m a    -- ^ Next expression
  } -> Sample m a
  -- | I don't know if this is going to work. It's likely to ruin the backtracking.
  Random :: {
    _metaInfo :: !m           -- ^ Opaque data, it may be used by the execution engine
  , _range :: !(r, r)         -- ^ Range of the random number
  , _next :: r -> Sample m a  -- ^ Next expression
  } -> Sample m a
  -- | Expression of zero probability. It should be discarded.
  Zero :: Sample m a

instance Functor (Sample m) where
  fmap f a@Val{_unVal=v} = a{_unVal=f v}
  fmap f a@Fork{..} = Fork {
      _next = \a -> fmap f $ _next a
    , ..
    }
  fmap _ Zero = Zero
  fmap f a@Guard{..} = Guard {
      _guarded = fmap f _guarded
    , ..
    }
  fmap f Random{..} = Random {
      _next = \a -> fmap f $ _next a
    , ..
    }

instance Applicative (Sample m) where
  pure a = Val a

  Val{_unVal=f} <*> a = fmap f a
  Fork{..} <*> a = Fork {
      _next = \x -> (_next x) <*> a
    , ..
    }
  Zero <*> _ = Zero
  Random{..} <*> a = Random { -- Crazy-ass weirdo haskeller, why did you define instance Random (->)?!!
      _next = \x -> (_next x) <*> a
    , ..
    }
  Guard{..} <*> a = Guard {
      _guarded = _guarded <*> a
    , ..
    }


instance Monad (Sample m) where
  Val{_unVal=v} >>= f = f v
  Fork{..}      >>= f = Fork {
       _next = \a -> _next a >>= f
     , ..
     }
  Zero          >>= _ = Zero
  Random{..}    >>= f = Random {
      _next = \a -> _next a >>= f
    , ..
    }
  Guard{..}     >>= f = Guard {
      _guarded = _guarded >>= f
    , ..
    }

  return = pure

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

  guard False = Zero
  guard True = Guard {
      _guarded = Val ()
    , _metaInfo = deFault
    }

  getRandomR r = Random {
      _metaInfo = deFault
    , _range = r
    , _next = \a -> Val a
    }

liftMaybe :: MonadVoretion m => Maybe a -> m a
liftMaybe (Just a) = guard True >> return a
liftMaybe Nothing  = guard False >> return undefined

class PickRandom c where
  pickRandom :: (MonadVoretion m) => c a -> m a

instance PickRandom [] where
  pickRandom l = choice $ zip [1..] l

{- | The simplest voretion engine which just evaluetes all possible
voretions of a program.

Limitations:
1. It doesn't support getRandomR calls!
2. It's O(eⁿ).
-}
noRandom :: Float -> Sample () b -> [(b, Float)]
noRandom ε = go 1
  where
    go _ Zero = []
    go n _ | n<ε = []
    go n Val{_unVal=v} = [(v, n)]
    go n Guard{_guarded=g} = go n g
    go n Fork{_bias=b, _next=f, _left=l, _right=r} = go (n*b) (f l) ++ go (n*(1-b)) (f r)
    go n Random{} = error "Random isn't supported by noRandom voretion engine"

{-    0.3
     /
  0.6\0.3
 /   
1     0.4
 \0.4/
     \0.0
-}

instance Default (Bool, Bool) where
  deFault = (False, False)

maxProbabilityFirst :: (RandomGen g) => g -> Sample (Bool, Bool) b -> b
maxProbabilityFirst rgen e = undefined -- TBD

{- | Metropolis-Hastings voretion engine.

Limitations:
It doesn't backtrack on failure. Effective search strategies 
can't be implemented in terms of a Markov chain voretion engine
like this.
-}
mhmcVE :: Float -> Sample () b -> [b]
mhmcVE ε x = undefined -- TBD

-- Fun:

histogram :: (Ord a) => Float -> Sample () a -> [(a, Float)]
histogram ε = M.toList . (foldr (uncurry $ M.insertWith (+)) M.empty) . noRandom ε

drawHistogram :: (Ord a) => Int -> Float -> Sample () a -> IO ()
drawHistogram height ε x =
  let
    h = histogram ε x
    m = maximum $ snd $ unzip h
    f (_,h) = ('|' : replicate n ' ')  ++ "*"
      where n = truncate $ (fromIntegral height) * h / m
  in do
    putStrLn $ (replicate (height+2) '-') ++ ">"
    mapM_ (putStrLn . f) h
    putStrLn "v"

{- | Examples:

1. Coin
@
coin = fork 0.5 Tails Heads
@

2. Geometric distribution
@
test :: (MonadVoretion m) => Int -> m Int
test n = do
  a <- fork 0.5 False True
  if a then
    test $ n+1
  else
    return n
@

3. Binomial distribution
@
test2 :: (MonadVoretion m) => Int -> Float -> m Int
test2 n p = sum <$> replicateM n (fork p 0 1)
@

-}
