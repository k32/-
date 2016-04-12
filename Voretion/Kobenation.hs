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
    TL Kob
    deriving (Eq, Show, Read)

{-             
Example kobenation:
 
  ______
 /    __\  ___
 |   /   \/   \  
 A black cat runs.

It's represented as:
Kob "cat" [Kob "A" [] [], Kob "black" [] []] [Kob "runs"]

It's seed-agnostic, for example:

Kob "A" [] [Kob "cat" [Kob "black" [] []] [Kob "runs" [] []]]

represents the same topology.
-}

data State = State {
      _threashold :: Float
    , _decay :: Float
    , _maxLinks :: Float
    }

-- Tvoretions (topological voretions)
-- Here we resolve links
tvoretion :: (MonadRandom m, MonadReader State m) => Ruleset -> Maybe LinkID -> Link -> m Kob
tvoretion ruleset uplink = undefined  . kobenate uplink . resolveMacros ruleset

-- Here we resolve ORs, multiconnectors and optionals
-- Rule's rval becomes flat after that
kobenate :: (MonadRandomm, MonadReader State m) => Link -> m ([LinkID], [LinkID])
kobenate link =
    case link of
         Link l
             | _linkDirection l == Down -> return ([l], [])
             | True                     -> return ([], [l])
         LinkOr l -> 
             let l' = selectUp l
             n <- getRandomR (0, length l' - 1)
             kobenate (l' !! n)

