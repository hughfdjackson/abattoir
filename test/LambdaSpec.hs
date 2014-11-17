module LambdaSpec (specs) where

import Test.Hspec
import Lambda

specs :: Spec
specs = describe "Lambda" $ do
  describe "show" $ do 
    it "should show Vars" $ do
      show (Var "x") `shouldBe` "x"

    it "should show Appliation" $ do
      show (Application (Var "x") (Var "y")) `shouldBe` "x y"

    it "should show Abstraction" $ do
      show (Abstraction "x" (Var "y")) `shouldBe` "(λx.y)"

  describe "eval" $ do
    it "should reduce vars no further" $ do
      let term =  Var "x"
      eval term `shouldBe` term

    it "should reduce Abstraction no further" $ do
      let term = (Abstraction "x" (Var "y"))
      eval term `shouldBe` term

    it "should reduce Application" $ do
      let c = (Abstraction "x" (Abstraction "y" (Var "x")))
      let term = (Application c (Var "z"))
      eval term `shouldBe` (Abstraction "y" (Var "z"))

    it "should reduce Application, respecting shadowing" $ do 
      let term = (Application (Abstraction "x" (Abstraction "x" (Var "x"))) (Var "z"))
      eval term `shouldBe` (Abstraction "x" (Var "x"))

