import LinkGrammar.AST
import LinkGrammar.Process
import System.IO
import Voretion.Kobenation

main = do
  hPutStrLn stderr "Loading ruleset..."
  ruleset <- loadRuleset "./ruleset.dat"
  hPutStrLn stderr "Done."
  let cfg = State {
              _threashold = 0.001
            , _decayₒ = 1000
            , _decayₘ= 60
            }
  kob <- doVoretion ruleset cfg undefined
  putStrLn $ humanize kob
