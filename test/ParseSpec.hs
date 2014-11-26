module ParseSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Parse (parse)
import Lambda

replace :: Char -> Char -> String -> String
replace x y = fmap (\c -> if c == x then y else c)


spec = describe "Parsing" $ do
  it "should be able to parse a shown arbitrary expression back to the same form" $ property $ \expr ->
    parse (show expr) === Right (expr :: Expr)

  it "should accept | instead of λ, cause, come-on.. who can really type that?" $ property $ \expr ->
    let shown = replace 'λ' '|' $  show expr
    in parse (shown) === Right (expr :: Expr)