module Main (main) where
import           System.IO

import           Commands
import           Control.Monad                    (unless, void)
import           Data.Functor                     ((<$>))
import           Data.List                        (intercalate, isPrefixOf,
                                                   stripPrefix)
import           Data.Maybe                       (fromMaybe)
import           Lambda                           (Expr, Synonyms, eval', evalSteps', synonymsEmpty)
import           System.Console.Haskeline
import           System.Console.Haskeline.History
import           Control.Monad.Trans.State
import           Control.Monad.Trans (lift)
import qualified Data.Map as Map

type ReplSession = StateT Synonyms (InputT IO) ()

outputResult :: Either String [String] -> ReplSession
outputResult e = case e of
  (Left error)  -> handleError error
  (Right lines) -> mapM_ (lift . outputStrLn) lines


handleInput :: Either String Command -> ReplSession
handleInput (Right command) = handleCommand command
handleInput (Left  error)   = handleError error >> runReplStep

handleCommand :: Command -> ReplSession
handleCommand command = do
    synonyms <- get
    case command of
      Quit                 -> quit
      Help                 -> help >> runReplStep
      Steps expr           -> outputResult (evalSteps' synonyms expr) >> runReplStep
      Step expr            -> outputResult (take 1 <$> evalSteps' synonyms expr) >> runReplStep
      Eval expr            -> outputResult ((:[]) . show <$> eval' synonyms expr) >> runReplStep
      Let name expr        -> do
        let expr' = eval' synonyms expr
        case expr' of
          (Right e) -> put (Map.insert name e synonyms)
          (Left f)  -> lift $ outputStrLn $ "ERROR: " ++ f
        runReplStep
      Unrecognised command -> do
        lift $ outputStrLn $ "could not recognize command " ++ command
        lift $ outputStrLn "try :help"
        runReplStep
  where
        quit = lift $ outputStrLn "Bye"
        help = lift $ outputStrLn "You ain't getting no help from me... yet"

handleError :: String -> ReplSession
handleError = lift . outputStrLn . ("ERROR: " ++)


runReplStep :: StateT Synonyms (InputT IO) ()
runReplStep = do
    minput <- lift $ getInputLine "> "
    maybe (return ()) (handleInput `fmap` Commands.parse) minput

main :: IO ()
main = runInputT defaultSettings (evalStateT runReplStep synonymsEmpty)



