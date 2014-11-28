module Commands (Commands.parse, Command(..)) where

import Text.Parsec as P
import Text.Parsec.String (Parser)
import Lambda (Expr(..))
import Parse (parseExpr)
import Control.Arrow (left)
import Control.Monad (liftM)

data Command = Eval Expr
             | Step Expr
             | Help
             | Quit
            deriving (Show, Eq)

parse :: String -> Either String Command
parse = left show . P.parse parseCommand  ""

parseCommand :: Parser Command
parseCommand = optWhitespace (colonCommands <|> parseEval)
  where colonCommands = char ':' >> (parseHelp <|> parseStep <|> parseQuit)

parseHelp :: Parser Command
parseHelp = string "help" >> return Help

parseQuit :: Parser Command
parseQuit = string "quit" >> return Quit

parseStep :: Parser Command
parseStep = do
  string "step "
  expr <- optWhitespace parseExpr
  return $ Step expr

parseEval :: Parser Command
parseEval = liftM Eval (optWhitespace parseExpr)


optWhitespace :: Parser a -> Parser a
optWhitespace = between spaces spaces
