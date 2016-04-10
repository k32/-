import LinkGrammar.Parsec
import LinkGrammar.AST
import Language.Preprocessor.Cpphs as CPP
import Control.Arrow
import Control.Monad.IO.Class
import Data.Either
import System.Environment
import Text.Printf
import Data.PrettyPrint
    
main = do
  cliopts <- getArgs
  options <- case CPP.parseOptions cliopts of
    Right x -> return x
    Left y -> error y
  let boolopts' = boolopts options
      options' = options {boolopts = boolopts' {locations = False}}
  str <- concat <$> mapM readFile (CPP.infiles options')
  s <- CPP.runCpphs options' "" str
  --print options'
  let ast = parseLink s
  case ast of
    Left x      -> putStrLn x
    Right rules -> do
             mapM (putStrLn . pretty) $ take 20 rules
             printf "...\nOk, imported %d rules\n" $ length rules
