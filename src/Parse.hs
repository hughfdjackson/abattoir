module Parse (parse, parseExpr, lexSynonym) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import LambdaWithSynonyms (Expr'(..))
import Data.Functor ((<$>))
import Control.Arrow (left)
import Control.Applicative ((<*))

parse :: String -> Either String Expr'
parse = transformError . P.parse parseAllExpr ""

transformError :: Either ParseError Expr' -> Either String Expr'
transformError = left show

parseAllExpr :: Parser Expr'
parseAllExpr = parseExpr <* eof

parseExpr :: Parser Expr'
parseExpr = (parseLambdaOrName <|> withParens parseExpr) `chainl1` return Ap'

parseLambdaOrName :: Parser Expr'
parseLambdaOrName = parseLambda <|> parseName <|> parseSynonym

parseLambda :: Parser Expr'
parseLambda = do
  lexLambda
  names <- lexNames
  lexDot
  expr <- parseExpr
  return $ foldr L' expr names

parseName :: Parser Expr'
parseName = V' <$> lexName

parseSynonym :: Parser Expr'
parseSynonym = S' <$> lexSynonym

--lexers
lexLambda = char 'λ' <|> char '\\'
lexDot = char '.'
lexName = oneOf ['a'..'z']
lexNames = many1 lexName
lexSynonym = oneOf $ ['A'..'Z'] ++ ['0'..'9']

--helpers
withParens :: Parser a -> Parser a
withParens = between (char '(') (char ')')