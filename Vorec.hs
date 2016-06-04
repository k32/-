{-# LANGUAGE NoMonomorphismRestriction #-}
import LinkGrammar.AST
import LinkGrammar.Process
import System.IO
import Voretion.Kobenation
import Data.PrettyPrint
import Data.TernaryTree
import Control.Monad
import Voretion.Config
import Options.Applicative
import Data.Binary
import qualified Data.Map as M (Map)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Lazy as BL (hGetContents)
import qualified Data.Vector as V (Vector, (!), map, fromList)
import qualified Data.Set as S (toList)
import LinkGrammar.Process
import Control.Lens
import Control.Monad.Voretion
import System.Random (newStdGen, getStdRandom, random, randomR, StdGen)
import Data.Char (toUpper)
import Data.List (isInfixOf)

import Debug.Trace

cliOptions :: Parser Config
cliOptions =
  let
    dopt = option auto $ long "doption"
                     <> value 0.5
                     <> short 'o'
                     <> help "Probability of optional links"
    dmul = option auto $ long "dmulti"
                     <> value 0.5
                     <> short 'm'
                     <> help "Probability of multiconnector links"
    ruleset = strOption $ long "ruleset"
                       <> short 'r'
                       <> help "Path to the ruleset (without extension)"
    epsilon = option auto $ long "epsilon"
                         <> short 'e'
                         <> help "Discard results with probability lesser than this"
                         <> value 0.001
    engine = option auto $ long "engine"
                       <> help "Choose execution engine (usedful for debugging)"
                       <> value MonteCarlo
  in Config <$> dopt
            <*> dmul
            <*> ruleset
            <*> option auto (long "costcost" <> value 0)
            <*> epsilon
            <*> engine

type RulesetIndex' = Ruleset (V.Vector Offset)

-- | Returns list of rules mating with a given one
matingRules :: Macros
            -> Handle
            -> RulesetIndex' 
            -> LinkID
            -> [Rule']
matingRules macros handle index {- offsetMap -} rule =
  let
    LinkID {_linkDirection = dir, _linkName = name} = rule
    
    idx = case dir of
            Plus  -> _downlinks index
            Minus -> _uplinks index
    offsetMap = _ruleset index
  in [ readRule macros handle (offsetMap V.! i)
     | (_, set) <- relaxedLookup (*<) True idx name
     , i <- S.toList set]

rulesVec :: Macros
         -> Handle
         -> RulesetIndex'
         -> V.Vector Rule'
rulesVec macros h = V.map (readRule macros h) . _ruleset

-- | Load rule from the ".rules" file. This operation is unsafe.
readRule :: Macros
         -> Handle
         -> Offset
         -> Rule'
readRule macros handle offset = unsafePerformIO $ do
  hSeek handle AbsoluteSeek offset
  deMacrify macros <$> decode <$> BL.hGetContents handle
{-# NOINLINE readRule #-}

debugOut :: ([NLPWord], Float)
         -> String
debugOut (str, p) = show p ++ "|" ++ unwords (map _nlpword str)

psychoza :: (StdGen -> [NLPWord])
         -> IO ()
psychoza f =
  let
    unUnderscore '_' = ' '
    unUnderscore x   = x
    
    capitalyze = (\(a:l) -> toUpper a : l) . dropWhile (==' ')
    
    psychoza' a ('=':t) = return $ ' ':(a++t)
    psychoza' a x | "MORPH-END" `isInfixOf` x = do
      let stems = ["питуш", "ворец", "зожат", "перда", "пехапе "]
      stem <- getStdRandom $ randomR (0, length stems - 1)
      return $ a ++ " " ++ ((stems!!stem) ++ drop (length "MORPH-END" + 1) x)
    psychoza' a t = return $ a++(' ':map unUnderscore t)
    
    humanyze t = do
      putStr =<< capitalyze <$> foldM psychoza' [] t
      putStr ". "
      paragraph <- getStdRandom random :: IO Float
      when (paragraph < 0.06) $
        putChar '\n'
      
  in do
    g₀ <- newStdGen
    humanyze $ map _nlpword $ f g₀
    psychoza f

main = do
  conf@Config {
      _pathToRuleset = file
    , _epsilon = ε
    , _engine = engine
    } <- execParser $ info (helper <*> cliOptions) fullDesc
  index <- (decodeFile $ file ++ ".index") :: IO RulesetIndex
  macros <- (decodeFile $ file ++ ".macros") :: IO Macros
  withFile (file ++ ".rules") ReadMode $ \hRules -> do
    let index' = index & ruleset %~ V.fromList
        matingRules' = matingRules macros hRules index'
        rulesVec' = rulesVec macros hRules index'
        voretion = natalyze conf matingRules' rulesVec'
    case engine of
      Deterministic ->
        mapM_ (putStrLn . debugOut) $ noRandom ε $ voretion
      MonteCarlo -> do
        psychoza $ stupidRandom voretion
