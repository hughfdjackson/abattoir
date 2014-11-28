module Parse (parse, parseExpr) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Lambda (Expr(..))
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
parseExpr = (parseLambdaOrName <|> withParens parseExpr) `chainl1` return Ap

parseLambdaOrName :: Parser Expr
parseLambdaOrName = parseLambda <|> parseName

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
lexLambda = char 'λ' <|> char '\\'

lexDot :: Parser Char
lexDot = char '.'

lexName :: Parser Char
lexName = oneOf ['a'..'z']

withParens :: Parser a -> Parser a
withParens = between (char '(') (char ')')