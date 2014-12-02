module LambdaWithSynonymsSpec (spec) where

import           Data.Either     ()
import           Test.Hspec
import           LambdaWithSynonyms
import           Lambda (Expr(..))
import           Data.Map as Map

spec :: Spec
spec = describe "LambdaWithSynonyms" $ do
  describe "Expr' show" $ do
    it "should show vars" $
      show (V' 'x') `shouldBe` "x"

    it "should show application" $
      show (Ap' (V' 'x') (V' 'y')) `shouldBe` "xy"

    it "should clarify right application with parens" $
      show (Ap' (V' 'x') (Ap' (V' 'y') (V' 'x'))) `shouldBe` "x(yx)"

    it "should show Abstraction" $
      show (L' 'x' (V' 'y')) `shouldBe` "(λx.y)"

    it "should show nested functions as a function of 'multiple arguments'" $ do
      show (L' 'x' (L' 'y' (V' 'y'))) `shouldBe` "(λxy.y)"
      show (L' 'x' (L' 'y' (L' 'z' (V' 'z')))) `shouldBe` "(λxyz.z)"

    it "should show symbols" $
      show (L' 'x' (L' 'y' (S' 'I'))) `shouldBe` "(λxy.I)"

  describe "show synonyms" $
    it "should show a list of synonyms" $
      showSynonyms (Map.fromList [('X', V 'x'), ('Y', V 'y')]) `shouldBe` unlines ["X = x", "Y = y"]

  describe "substituteSynonyms" $ do
    it "should substitute in synonyms" $ do
      let id = L 'x' (V 'x')
      let synonyms = Map.fromList [('I', id)]
      substituteSynonyms synonyms (Ap' (S' 'I') (S' 'I')) `shouldBe` Right (Ap id id)

    it "should return a failure if a synonym can't be resolved" $
     substituteSynonyms Map.empty (Ap' (S' 'I') (S' 'I')) `shouldBe` Left "Cannot find synonym I"
