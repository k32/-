{-# LANGUAGE FlexibleContexts #-}
module Voretion.Kobenation
    (
      Kob(..)
    , humanize
    , doVoretion
    , State(..)
    ) where

import Prelude hiding (foldl1)
import Control.Monad.Random
import Control.Monad.Reader
import LinkGrammar.AST
import LinkGrammar.Process
import Control.Arrow
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Foldable
import Debug.Trace

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

humanize :: Kob -> String
humanize (Kob a (d, u)) = (d >>= humanize) ++ a ++ (u >>= humanize)

doVoretion :: Ruleset -> State -> String -> IO Kob
doVoretion ruleset cfg vseed = evalRandIO $ (flip runReaderT) cfg $ do
                                 i <- getRandomR (0, V.length $ _rules ruleset)
                                 let (Rule' lval rule) = (_rules ruleset) V.! i
                                 j <- getRandomR (0, length lval - 1)
                                 tvoretion ruleset $ TL (_nlpword $ lval !! j) rule
  where vseed' = undefined-- TODO: get it right
        nrules = V.length $ _rules ruleset 

-- Tvoretions (topological voretions)
-- Here we kobenate and resolve links
tvoretion :: (MonadRandom m, MonadReader State m) => Ruleset -> Kob -> m Kob
tvoretion ruleset =  go []
  where
    go g (TL τ ł) = do
      (δ₀, υ₀) <- kobenate g ł -- TODO: not enough psihoza... make it pointfree
      δ₁ <- mapM (f Minus) δ₀
      υ₁ <- mapM (f Plus) υ₀
      return $ Kob τ (δ₁, υ₁)
             
    f :: (MonadRandom m, MonadReader State m) => LinkDirection -> LinkName -> m Kob
    f ld ln = do
      let rr = (_index ruleset) M.! (LinkID ln ld)
          n  = length rr
      i <- getRandomR (0, n-1)
      let (idx1, idx2) = rr !! i
          (Rule' lval link) = (_rules ruleset) V.! idx1
          nlval = length lval
      j <- getRandomR (0, nlval-1)
      go idx2 $ TL (_nlpword $ lval !! j) link

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
    then trace ("Opt,t=" ++ show t) $ local (\s@State{_threashold=t₀, _decayₒ=δ}->s{_threashold=t₀/δ}) a
    else return d
      

vorecMul :: (MonadRandom m, MonadReader State m) => m a -> m [a]
vorecMul a = do
  --x <- getRandom
  t <- asks _threashold
  if False -- x < t
    then trace ("Mul,t=" ++ show t) $ local (\s@State{_threashold=t₀, _decayₘ=δₘ}->s{_threashold=t₀/δₘ}) $ 
               ((:) <$> a <*> vorecMul a)
    else return []
