module Commands (Commands.parse, Command(..), commands, helpText) where

import Text.Parsec as P
import Text.Parsec.String (Parser)
import LambdaWithSynonyms (Expr'(..), Name)
import Parse (parseExpr, lexSynonym)
import Control.Arrow (left)
import Control.Monad (liftM)
import Control.Applicative ((<*>))
import Data.Functor ((<$>))
import Data.List (sort)

data Command = Eval Expr'
             | Step Expr'
             | Steps Expr'
             | Help
             | Quit
             | Let Name Expr'
             | Unrecognised String
             | ShowSynonyms
            deriving (Show, Eq)

commands = sort [":help", ":quit", ":step", ":steps", ":let", ":synonyms"]
helpText = "You ain't getting no help from me... yet"

parse :: String -> Either String Command
parse = left show . P.parse parseCommand  ""

parseCommand :: Parser Command
parseCommand = optWhitespace (colonCommands <|> parseEval)
  where colonCommands = char ':' >> (parseHelp
                                 <||> parseQuit
                                 <||> parseStep
                                 <||> parseSteps
                                 <||> parseLet
                                 <||> parseShowSynonyms
                                 <||> parseUnrecognised)

parseHelp :: Parser Command
parseHelp = string "help" >> return Help

parseQuit :: Parser Command
parseQuit = string "quit" >> return Quit

parseStep :: Parser Command
parseStep = liftM Step (string "step" >> many1 space >> optWhitespace parseExpr)

parseSteps :: Parser Command
parseSteps = liftM Steps (string "steps" >> many1 space >> optWhitespace parseExpr)

parseLet :: Parser Command
parseLet = Let <$> (string "let" >> many1 space >> lexSynonym) <*> (many1 space >> parseExpr)

parseShowSynonyms :: Parser Command
parseShowSynonyms = string "synonyms" >> return ShowSynonyms

parseUnrecognised :: Parser Command
parseUnrecognised = liftM Unrecognised $ (":" ++) <$> many anyChar

parseEval :: Parser Command
parseEval = liftM Eval (optWhitespace parseExpr)

optWhitespace :: Parser a -> Parser a
optWhitespace = between spaces spaces

(<||>) :: Parser a -> Parser a -> Parser a
a <||> b = try a <|> try b