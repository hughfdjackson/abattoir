module SynonymsSpec (specs) where

import qualified Synonyms as S
import Lambda
import Test.Hspec
import Data.Map as Map

specs :: Spec
specs = describe "Synonyms" $
  describe "substituteSynonyms" $ do
    it "should substitute in synonyms" $ do
      let id = L 'x' (V 'x')
      let synonyms = Map.fromList [('I', id)]
      S.substituteSynonyms synonyms (S.Ap (S.S 'I') (S.S 'I')) `shouldBe` Right (Ap id id)

    it "should return a failure if a synonym can't be resolved" $
      S.substituteSynonyms Map.empty (S.Ap (S.S 'I') (S.S 'I')) `shouldBe` Left "Cannot find synonym I"