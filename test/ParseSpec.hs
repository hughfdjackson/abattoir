module ParseSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Parse (parse)
import Lambda

instance Arbitrary Expr' where
   arbitrary = do
     n <- choose (0, 3) :: Gen Int
     case n of
       0 -> do name <- possibleNames
               return $ V' name

       1 -> do name <- possibleSynonyms
               return $ S' name

       2 -> do name <- possibleNames
               expr <- arbitrary
               return $ L' name expr

       _ -> do expr  <- arbitrary
               expr' <- arbitrary
               name  <- possibleNames
               return $ Ap' (L' name expr) expr'
     where possibleNames = elements ['a'..'z']
           possibleSynonyms = elements $ ['A'..'Z'] ++ ['0'..'9']


replace :: Char -> Char -> String -> String
replace x y = fmap (\c -> if c == x then y else c)

spec :: SpecWith ()
spec = describe "Parsing" $ do
  it "should be able to parse a shown arbitrary expression back to the same form" $ property $ \expr ->
    parse (show expr) === Right (expr :: Expr')

  it "should accept \\ instead of λ, cause, come-on.. who can really type that?" $ property $ \expr ->
    let shown = replace 'λ' '\\' $  show expr
    in parse shown === Right (expr :: Expr')
