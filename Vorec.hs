import LinkGrammar.AST
import LinkGrammar.Process
import System.IO
import Voretion.Kobenation

main = do
  hPutStrLn stderr "Loading ruleset..."
  ruleset <- loadRuleset "./ruleset.dat"
  hPutStrLn stderr "Done."
  
