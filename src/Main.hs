module Main (main) where
import           System.IO

import           Commands
import           Control.Monad                    (unless, void)
import           Data.Functor                     ((<$>))
import           Data.List                        (intercalate, isPrefixOf,
                                                   stripPrefix)
import           Data.Maybe                       (fromMaybe)
import           Lambda                           (Expr, eval, evalSteps)
import           System.Console.Haskeline
import           System.Console.Haskeline.History


outputResult :: Either String [String] -> InputT IO ()
outputResult e = case e of
  (Left error)  -> handleError error
  (Right lines) -> mapM_ outputStrLn lines


handleInput :: Either String Command -> InputT IO ()
handleInput (Right command) = handleCommand command
handleInput (Left  error)   = handleError error >> runReplStep

handleCommand :: Command -> InputT IO ()
handleCommand command = case command of
    Quit                 -> quit
    Help                 -> help >> runReplStep
    Steps expr           -> outputResult (evalSteps expr) >> runReplStep
    Step expr            -> outputResult (take 1 <$> evalSteps expr) >> runReplStep
    Eval expr            -> outputResult ((:[]) . show <$> eval expr) >> runReplStep
    Unrecognised command -> do
      outputStrLn $ "could not recognize command " ++ command
      outputStrLn "try :help"
      runReplStep
  where quit = outputStrLn "Bye"
        help = outputStrLn "You ain't getting no help from me... yet"

handleError :: String -> InputT IO ()
handleError = outputStrLn . ("ERROR: " ++)


runReplStep :: InputT IO ()
runReplStep = do
    minput <- getInputLine "> "
    maybe (return ()) (handleInput `fmap` Commands.parse) minput

main :: IO ()
main = runInputT defaultSettings runReplStep



