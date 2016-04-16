{-# LANGUAGE FlexibleContexts #-}
module Voretion.Kobenation
    (
      Kob(..)
    , tvoretion
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
tvoretion :: (MonadRandom m, MonadReader State m) => Ruleset -> Link' Full -> m Kob
tvoretion ruleset = undefined

-- Here we resolve ORs, multiconnectors and optionals
-- Rule's rval becomes flat after that
kobenate :: (MonadRandom m, MonadReader State m) => Link' Full -> m ([LinkName], [LinkName])
kobenate = undefined
