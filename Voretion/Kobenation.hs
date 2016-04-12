module Voretion.Kobenation
    (
      Kob(..)
    , voretion
    ) where

import Control.Monad.Random
import Control.Monad.Reader
import LinkGrammar.AST
import LinkGrammar.Process
    
-- Kobenation    
data Kob = Kob
    {
      _text :: String
    , _down
    , _up   :: [Kob]
    } |
    KL Link
    deriving (Eq, Show, Read)             

kobenate :: (MonadRandom m) => Ruleset -> String -> m String
kobenate ruleset seed = undefined

-- Here we resolve ORs, multiconnectors and optional links
-- Rule's rval becomes flat, so we can represent it by two lists
-- It's based on the assumption that :&: is associative
voretion :: (MonadRandom m, MonadReader Float m) => Maybe LinkID -> Link -> m ([LinkID], [LinkID])
voretion uplink link = v [] [] link
    where
        v d u (Link l)
          | _linkDirection l == Down = return (d ++ [l], u)
          | True                     = return (d, u ++ [l])
        v d u (a :&: b) = do
          (d_a, u_a) <- v d u a
          (d_b, u_b) <- v d u b
          return (d_a ++ d_b, u_a ++ u_b)
        v d u (LinkOr l) = do
          n <- getRandomR (0, length l - 1)
          v d u (l !! n)
          
