import Control.Arrow
import Control.Applicative
import Control.Monad.IO.Class
import Data.Either
import Data.PrettyPrint
import Language.Preprocessor.Cpphs as CPP
import LinkGrammar.AST
import LinkGrammar.Parsec
import System.Environment
import Text.Printf

processFile o f = parseLink <$> (CPP.runCpphs o f =<< readFile f)
    
main = do
  cliopts <- getArgs
  options <- case CPP.parseOptions cliopts of
    Right x -> return x
    Left y -> error y
  let boolopts' = boolopts options
      options' = options {boolopts = boolopts' {locations = False}}
  files <- mapM (processFile options') $ CPP.infiles options'
  --print options'
  let ast = foldr (liftA2 (++)) (pure []) files
  case ast of
    Left x      -> putStrLn x
    Right rules -> do
             mapM (putStrLn . pretty) $ take 20 rules
             printf "...\nOk, imported %d rules\n" $ length rules
