import LinkGrammar.AST
import LinkGrammar.Process
import System.IO
import System.Environment
import Voretion.Kobenation

main = do
  ruleFile <- head <$> getArgs
  hPutStrLn stderr "Loading ruleset..."
  ruleset <- loadRuleset ruleFile
  hPutStrLn stderr "Done."
  let cfg = State {
              _threashold = 0.001
            , _decayₒ = 1000
            , _decayₘ= 60
            }
  kob <- doVoretion ruleset cfg undefined
  putStrLn $ show kob
