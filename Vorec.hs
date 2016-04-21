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
  -- print ruleset
  let cfg = State {
              _threashold = 0.9
            , _decayₒ = 10
            , _decayₘ= 6
            }
  kob <- doVoretion ruleset cfg undefined
  print kob
  putStrLn $ humanize kob
