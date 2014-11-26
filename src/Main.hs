module Main (main) where
import System.IO

import Parse (parse)
import Lambda (eval, Expr)
import Control.Monad (unless)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: String -> IO ()
evalAndPrint expr = do
  let result = parse expr >>= eval
  putStrLn $ resultToString result

resultToString :: Either String Expr -> String
resultToString e = case e of
  (Left f)  -> "ERROR:" ++ f
  (Right r) -> show r

until' :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until' pred prompt action = do
   result <- prompt
   unless (pred result) $
      action result >> until' pred prompt action

main = until' (== "quit") (readPrompt "> ") evalAndPrint


