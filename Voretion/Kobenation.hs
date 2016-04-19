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
import Control.Arrow    

-- Kobenation    
data Kob = Kob
    {
      _text :: String
    , _down
    , _up   :: [Kob]
    } |
    TL String Link
    deriving (Eq, Show)

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
tvoretion :: (MonadRandom m, MonadReader State m) => Ruleset -> Kob -> m Kob
tvoretion ruleset = undefined

-- Here we resolve ORs, multiconnectors and optionals
-- Rule's rval becomes flat after that
kobenate :: (MonadRandom m, MonadReader State m) => [Int] -> Link -> m ([LinkName], [LinkName])
kobenate idx (Node t c) =
    case t of
      Optional ->
          case idx of
            (_:idx') ->
                vorecOpt $ kobenate idx' $ head c
            [] ->
                kobenate [] $ head c
      LinkAnd -> do
             let ρ (x, φ) = case idx of
                              (α:idx')
                                  | φ == α -> (idx', x)
                              _ ->
                                  ([], x)
                 l' = map ρ (zip c [0..]) 
             (concat *** concat . reverse) <$> unzip <$> mapM (uncurry kobenate) l'
      LinkOr -> do
          let n = length c
          i <- getRandomR (0,n-1)
          kobenate [] $ c !! i
      Link (LinkID n d) ->
          case d of
            Plus -> return ([], [n])
            Minus -> return ([n], [])
      MultiConnector ->
          -- TODO: implement me
          --(concat *** concat . reverse) <$> unzip <$> (vorecMul $ kobenate [] $ head c)
          return ([], [])
          
vorecOpt :: (MonadRandom m, MonadReader State m) => m a -> m a
vorecOpt = undefined

vorecMul :: (MonadRandom m, MonadReader State m) => m a -> m [a]
vorecMul = undefined
           
