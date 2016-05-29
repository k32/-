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
  in Config <$> dopt
            <*> dmul
            <*> ruleset
            <*> option auto (long "costcost" <> value 0)

type RulesetIndex' = Ruleset (V.Vector Offset)

-- | Returns list of rules mating with a given one
matingRules :: Handle
            -> RulesetIndex' 
            -> LinkID
            -> [Rule']
matingRules handle index {- offsetMap -} rule =
  let
    LinkID {_linkDirection = dir, _linkName = name} = rule
    
    idx = case dir of
            Plus  -> _downlinks index
            Minus -> _uplinks index
    offsetMap = _ruleset index
  in [ readRule handle (offsetMap V.! i)
     | (_, set) <- relaxedLookup (*<) True idx name
     , i <- S.toList set]                 

rulesVec :: Handle
         -> RulesetIndex'
         -> V.Vector Rule'
rulesVec h = V.map (readRule h) . _ruleset

-- | Load rule from the ".rules" file. This operation is unsafe.
readRule :: Handle
         -> Offset
         -> Rule'
readRule handle offset = unsafePerformIO $ do
  hSeek handle AbsoluteSeek offset
  decode <$> BL.hGetContents handle
{-# NOINLINE readRule #-}

humanize :: [NLPWord]
         -> String
humanize = unwords . map _nlpword

main = do
  conf@Config {
      _pathToRuleset = file
    } <- execParser $ info (helper <*> cliOptions) fullDesc
  index <- (decodeFile $ file ++ ".index") :: IO RulesetIndex
  macros <- (decodeFile $ file ++ ".macros") :: IO Macros
  withFile (file ++ ".rules") ReadMode $ \hRules -> do
    let index' = index & ruleset %~ V.fromList
        matingRules' = matingRules hRules index'
        rulesVec' = rulesVec hRules index'
        runVorec = noRandom 0.001
    print $ runVorec $ natalyze matingRules' macros rulesVec'
