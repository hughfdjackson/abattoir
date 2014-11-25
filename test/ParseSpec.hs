module ParseSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Parse (parse)
import Lambda

spec = describe "Parsing" $
  it "should be able to parse a shown arbitrary expression back to the same form" $ property $ \expr ->
  parse (show expr) === Right (expr :: Expr)