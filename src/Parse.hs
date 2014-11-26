module Parse (parse) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Lambda (Expr(..), Name)
import Data.Functor ((<$>))
import Control.Arrow (left)
import Control.Applicative ((<*))

parse :: String -> Either String Expr
parse = transformError . P.parse parseAllExpr ""

transformError :: Either ParseError Expr -> Either String Expr
transformError = left show

parseAllExpr :: Parser Expr
parseAllExpr = parseExpr <* eof

parseExpr :: Parser Expr
parseExpr = parseLambdaOrName `chainl1` parseAp

parseAp :: Parser (Expr -> Expr -> Expr)
parseAp = return Ap

parseLambdaOrName :: Parser Expr
parseLambdaOrName = withOptParens $ parseLambda <|> parseName

parseLambda :: Parser Expr
parseLambda = do
  lexLambda
  name <- lexName
  lexDot
  expr <- parseExpr
  return $ L name expr

parseName :: Parser Expr
parseName = V <$> lexName

-- lex
lexLambda :: Parser Char
lexLambda = char 'λ'

lexDot :: Parser Char
lexDot = char '.'

lexName :: Parser Char
lexName = oneOf ['a'..'z']

withOptParens :: Parser a -> Parser a
withOptParens parser = between (char '(') (char ')') parser <|> parser