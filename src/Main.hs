module Main (main) where
import System.IO

import Lambda (eval, evalSteps, Expr)
import Control.Monad (unless, void)
import Data.List (intercalate, isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Functor ((<$>))
import System.Console.Haskeline
import Commands


outputResult :: Either String [String] -> InputT IO ()
outputResult e = case e of
  (Left error)  -> handleError error
  (Right lines) -> mapM_ outputStrLn lines


handleInput :: Either String Command -> InputT IO ()
handleInput (Right command) = handleCommand command
handleInput (Left  error)   = handleError error >> runReplStep

handleCommand :: Command -> InputT IO ()
handleCommand command = case command of
    Quit -> quit
    Help -> help >> runReplStep
    Step expr -> outputResult (evalSteps expr) >> runReplStep
    Eval expr -> outputResult ((:[]) . show <$> eval expr) >> runReplStep
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



