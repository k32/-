import LinkGrammar.AST
import LinkGrammar.Process
import System.IO
import System.Environment
import Voretion.Kobenation
import Data.PrettyPrint
import Control.Monad

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
  replicateM 100 $ do
         kob <- doVoretion ruleset cfg undefined
  -- putStrLn $ pretty kob
         putStrLn $ humanize kob
  return ()
