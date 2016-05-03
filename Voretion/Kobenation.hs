{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Voretion.Kobenation
    (
      Kob(..)
    , humanize
    , doVoretion
    , State(..)
    ) where

-- import Control.Monad.Random
import Control.Monad.Reader
import LinkGrammar.AST
import LinkGrammar.Process
import Control.Arrow
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.List
import Debug.Trace
import Data.PrettyPrint

-- Kobenation    
data Kob = Kob
    {
      _Τ :: String
    , _Æ :: ([Kob], [Kob])
    } |
    TL String Link
    deriving (Eq, Show)

instance PrettyPrint Kob where
  pretty = drawTree . go ""
    where go x (Kob a (d, u)) = Node (a ++ x) $ map (go "-") d ++ map (go "+") u

data State = State {
    _threashold :: Float
  , _decayₒ
  , _decayₘ :: Float
  }

humanize :: Kob -> String
humanize (Kob a (d, u)) = unwords [(unwords $ map humanize d), a, (unwords $ map humanize u)]

doVoretion :: Ruleset -> State -> String -> IO Kob
doVoretion ruleset cfg vseed = evalRandIO $ (flip runReaderT) cfg $ do
                                 i <- getRandomR (0, (V.length $ _rules ruleset) - 1)
                                 let (Rule' lval rule) = (_rules ruleset) V.! i
                                 j <- getRandomR (0, length lval - 1)
                                 tvoretion ruleset $ TL (_nlpword $ lval !! j) rule
  where vseed' = undefined -- TODO: get it right
        nrules = V.length $ _rules ruleset 

-- Tvoretions (topological voretions)
-- Here we kobenate and resolve links
tvoretion :: (MonadRandom m, MonadReader State m) => Ruleset -> Kob -> m Kob
tvoretion ruleset =  go []
  where
    go g (TL τ ł) = do
      (δ₀, υ₀) <- kobenate g ł -- TODO: not enough psihoza... make it pointfree
      δ₁ <- mapM (f Plus) δ₀
      υ₁ <- mapM (f Minus) υ₀
      return $! Kob τ (δ₁, υ₁)
             
    f :: (MonadRandom m, MonadReader State m) => LinkDirection -> LinkName -> m Kob
    f ld ln = do
      let rr = trace ("!! Looking up " ++ show (LinkID ln ld)) $ (_index ruleset) M.! (LinkID ln ld)
          n  = length rr
      i <- getRandomR (0, n-1)
      let (idx1, idx2) = rr !! i
          (Rule' lval link) = (_rules ruleset) V.! idx1
          nlval = length lval
      j <- getRandomR (0, nlval-1)
      let t''' = TL (_nlpword $ lval !! j) link
      --trace ("!!!!!!\n" ++ show t'''  ++ "\n!!!!!\n") $ return ()
      go idx2 t'''

-- Here we resolve ORs, multiconnectors and optionals
-- Rule's rval becomes flat after that, only &s and linkIDs stand
kobenate :: (MonadRandom m, MonadReader State m) => [Int] -> Link -> m ([LinkName], [LinkName])
kobenate !idx (Node !t !c) =
    case t of
      Optional ->
          case idx of
            (_:idx') ->
                kobenate idx' $ head c
            [] ->
                vorecOpt ([], []) $ kobenate [] $ head c
      LinkAnd -> do
             let ρ (x, φ) = case idx of
                              (α:idx')
                                  | φ == α -> (idx', x)
                              _ ->
                                  ([], x)
                 l' = map ρ (zip c [0..])
             (concat *** concat) <$> unzip <$> mapM (uncurry kobenate) l'
      LinkOr ->
          case idx of
            [] -> do
              let n = length c
              i <- getRandomR (0,n-1)
              kobenate [] $ c !! i
            (α:idx') ->
              kobenate idx' $ c !! α
      Link _ (LinkID n d)
          | null idx ->
              case d of
                Plus -> return ([], [n])
                Minus -> return ([n], [])
          | True ->
              return ([], [])
      MultiConnector ->
          (concat *** concat) <$> unzip <$> (vorecMul $ kobenate (drop 1 idx) $ head c)
      Cost _ ->
          kobenate (drop 1 idx) $ head c

vorecOpt :: (MonadRandom m, MonadReader State m) => a -> m a -> m a
vorecOpt !d !a = do
  x <- getRandom
  t <- asks _threashold
  if x < t
    then {- trace ("Opt,t=" ++ show t) $-} local (\s@State{_threashold=t₀, _decayₒ=δ}->s{_threashold=t₀/δ}) a
    else return d
      

vorecMul :: (MonadRandom m, MonadReader State m) => m a -> m [a]
vorecMul !a = do
  x <- getRandom
  t <- asks _threashold
  if x < t
    then {- trace ("Mul,t=" ++ show t) $ -}
           local (\s@State{_threashold=t₀, _decayₘ=δₘ}->s{_threashold=t₀/δₘ}) $ 
               ((:) <$> a <*> vorecMul a)
    else return []
