module Main (main) where
import System.IO

import Parse (parse)
import Lambda (eval, evalSteps, Expr)
import Control.Monad (unless, void)
import Data.List (intercalate, isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import System.Console.Haskeline

evalAndShow :: String -> String
evalAndShow expr = resultToString $ showAndWrapResult $ parse expr >>= eval
  where showAndWrapResult = fmap (\a -> [show a])

evalStepAndShow :: String -> String
evalStepAndShow input = resultToString $ do
  expr <- parse input
  steps <- evalSteps expr
  result <- eval expr
  return $ steps ++ [show result]

resultToString :: Either String [String] -> String
resultToString e = case e of
  (Left f)  -> "ERROR:" ++ f
  (Right r) -> unlines r

quit :: InputT IO ()
quit = outputStrLn "Bye"

help :: InputT IO ()
help = outputStrLn "You ain't getting no help from me... yet"

runRepl :: IO ()
runRepl = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "> "
        case minput of
            Nothing -> return ()
            Just ":q" -> quit
            Just ":quit" -> quit
            Just ":help" -> help >> loop
            Just input -> handleInput input


    handleInput :: String -> InputT IO ()
    handleInput input
      | ":steps " `isPrefixOf` input = do
          let input' = fromMaybe "" (stripPrefix ":steps " input)
          outputStrLn $ evalStepAndShow input'
          loop
      | otherwise = do
          outputStrLn (evalAndShow input)
          loop


main :: IO ()
main = runRepl


