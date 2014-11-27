module Commands (parse, Commands(..)) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Lambda (Expr(..))

data Commands = Eval Expr
              | Step Expr






optWhitespace :: Parser ()
optWhitespace = many spaces