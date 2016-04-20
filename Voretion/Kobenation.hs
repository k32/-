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
      _Τ :: String
    , _Æ :: ([Kob], [Kob])
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
    , _decayₒ
    , _decayₘ :: Float
    }

-- Tvoretions (topological voretions)
-- Here we kobenate and resolve links
tvoretion :: (MonadRandom m, MonadReader State m) => Ruleset -> Kob -> m Kob
tvoretion ruleset (TL τ ł) = do
    (δ₀, υ₀) <- kobenate [] ł -- TODO: not enough psihoza... make it pointfree
    δ₁ <- mapM (f Minus) δ₀
    υ₁ <- mapM (f Plus) υ₀
    return $ Kob τ (δ₁, υ₁)
  where
    f :: (MonadRandom m, MonadReader State m) => LinkDirection -> LinkName -> m Kob
    f ϧ

-- Here we resolve ORs, multiconnectors and optionals
-- Rule's rval becomes flat after that, only &s and linkIDs stand
kobenate :: (MonadRandom m, MonadReader State m) => [Int] -> Link -> m ([LinkName], [LinkName])
kobenate idx (Node t c) =
    case t of
      Optional ->
          case idx of
            (_:idx') ->
                vorecOpt ([], []) $ kobenate idx' $ head c
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
      LinkOr ->
          case idx of
            [] -> do
              let n = length c
              i <- getRandomR (0,n-1)
              kobenate [] $ c !! i
            (α:idx') ->
              kobenate idx' $ c !! α
      Link (LinkID n d) ->
          case d of
            Plus -> return ([], [n])
            Minus -> return ([n], [])
      MultiConnector ->
          (concat *** concat . reverse) <$> unzip <$> (vorecMul $ kobenate (drop 1 idx) $ head c)
      Cost _ -> 
          kobenate (drop 1 idx) $ head c

vorecOpt :: (MonadRandom m, MonadReader State m) => a -> m a -> m a
vorecOpt d a = do
  x <- getRandom
  t <- asks _threashold
  if x < t
    then local (\s@State{_threashold=t₀, _decayₒ=δ}->s{_threashold=t₀*δ}) a
    else return d
      

vorecMul :: (MonadRandom m, MonadReader State m) => m a -> m [a]
vorecMul a = do
  x <- getRandom
  t <- asks _threashold
  if x < t
    then local (\s@State{_threashold=t₀, _decayₘ=δₘ}->s{_threashold=t₀*δₘ}) $ 
               (:) <$> a <*> vorecMul a
    else return []

           
