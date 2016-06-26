import Control.Arrow
import Control.Applicative
import Control.Monad.IO.Class
import Data.Either
import Data.PrettyPrint
import LinkGrammar.AST
import LinkGrammar.Parsec
import LinkGrammar.Process
import System.Environment
import Text.Printf
import Options.Applicative

data CliOptions = CliOptions {
    _outfile :: String
  , _infile :: String
  }
  
cliOptions :: Parser CliOptions
cliOptions = CliOptions <$>
      strOption (long "output"
              <> metavar "OUTFILE"
              <> short 'o'
              <> help "Output file names")
  <*> argument str (metavar "GRAMMAR"
              <> help "Input file")

-- processFile o f = parseLink <$> (CPP.runCpphs o f =<< readFile f)

main :: IO ()
main = do
  cliopts <- execParser $ info (helper <*> cliOptions) fullDesc
  ast <- parseLink <$> readFile (_infile cliopts)
  case ast of
    Left x      -> putStrLn x
    Right rules -> do
             makeRuleset (_outfile cliopts) rules
             --mapM (putStrLn . pretty) $ take 20 rules
             --mapM (putStrLn . drawTree . fmap show . _links) $ take 20 rules
             printf "Ok, imported %d rules\n" $ length rules
