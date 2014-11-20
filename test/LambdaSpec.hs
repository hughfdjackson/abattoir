module LambdaSpec (specs) where

import Test.Hspec
import Test.QuickCheck
import Lambda

names :: Gen Char
names = elements ['a'..'z']

instance Arbitrary Expr where
  arbitrary = do
    n <- choose (0, 2) :: Gen Int
    case n of
      0 -> do name <- names
              return $ v name
              
      1 -> do name <- names
              expr <- arbitrary
              return $ l name expr

      2 -> do expr  <- arbitrary
              expr' <- arbitrary
              name  <- names
              return $ ap (l name expr) expr'

specs :: Spec
specs = describe "Lambda" $ do
  -- describe "show" $ do 
    -- it "should show vars" $ do
    --   show (v 'x') `shouldBe` "x"

    -- it "should show appliation" $ do
    --   show (ap (v 'x') (v 'y')) `shouldBe` "xy"

    -- it "should show Abstraction" $ do
    --   show (l 'x' (v 'y')) `shouldBe` "(λx.y)"
  describe "reduced form" $ do
    it "should tell whether a form is fulled reduced" $ do
      isReducedForm (ap (l 'x' (v 'x')) (v 'y')) `shouldBe` False
      isReducedForm (l 'x' (ap (v 'x') (v 'y'))) `shouldBe` True
      isReducedForm (v 'x')                      `shouldBe` True

  describe "eval" $ do
    it "should eval vars to themselves" $ do
      eval (v 'x') `shouldBe` (v 'x')

    it "should eval abstraction to itself" $ do
      eval (l 'x' (v 'y')) `shouldBe` (l 'x' (v 'y'))

    it "should evaluate identity fn to obey identity law" $ property $ \expr ->
      isReducedForm expr ==> eval (ap (l 'x' (v 'x')) expr) == (expr :: Expr)

    it "should evaluate constant fn to obey constant law" $ property $ \expr ->
      isReducedForm expr ==> eval (ap (ap (l 'x' (l 'y' (v 'x'))) expr) (v 'z')) == (expr :: Expr)
