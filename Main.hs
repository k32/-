import LinkGrammar.Parser
import LinkGrammar.Tokenize
import LinkGrammar.AST
import Language.Preprocessor.Cpphs
import Control.Arrow
import Control.Monad.IO.Class
import Data.Either
import System.Environment
import Text.Printf

main = do
  cliopts <- getArgs
  options <- case parseOptions cliopts of
    Right x -> return x
    Left y -> error y
  str <- concat <$> mapM readFile (infiles options)
  s <- runCpphs options "" str
  let tokens = tokenize s
      ast    = parse =<< (drop 3) <$> tokens
  --print tokens
  case ast of
    Left x -> putStrLn $ "Error: " ++ x
    Right rules -> printf "Ok, imported %d rules\n" $ length rules
