module LambdaSpec (specs) where

import           Data.Either     ()
import           Data.Set        as Set
import           Lambda
import           Test.Hspec
import           Test.QuickCheck


specs :: Spec
specs = describe "Lambda" $ do
  describe "show" $ do
    it "should show vars" $
      show (V 'x') `shouldBe` "x"

    it "should show application" $
      show (Ap (V 'x') (V 'y')) `shouldBe` "xy"

    it "should clarify right application with parens" $
      show (Ap (V 'x') (Ap (V 'y') (V 'x'))) `shouldBe` "x(yx)"

    it "should show Abstraction" $
      show (L 'x' (V 'y')) `shouldBe` "(λx.y)"

  describe "renameBoundTo" $ do
    it "should rename to the next alphabetical letter *not* bound in the current expression" $ do
      let xToA =  'x' `renameBoundTo` 'a'
      let expr  = L 'x' (L 'y' (L 'z' (Ap (Ap (V 'x') (V 'y')) (V 'z'))))

      xToA expr `shouldBe` L 'a' (L 'y' (L 'z' (Ap (Ap (V 'a') (V 'y')) (V 'z'))))

    it "It should not rename a free variable" $ do
      let xToA =  'x' `renameBoundTo` 'a'
      let expr = L 'y' (Ap (L 'x' (V 'x'))  (V 'x'))

      xToA expr `shouldBe` L 'y' (Ap (L 'a' (V 'a')) (V 'x'))

  describe "renameBoundWithout" $
    it "should renameBoundTo the first available name in the alphabet given an empty blacklist" $ do
      let expr  = L 'x' (L 'y' (L 'z' (Ap (Ap (V 'x') (V 'y')) (V 'z'))))
      let expr' = L 'y' (Ap (L 'x' (V 'x'))  (V 'x'))

      renameBoundWithout Set.empty 'x' expr  `shouldBe` L 'a' (L 'y' (L 'z' (Ap (Ap (V 'a') (V 'y')) (V 'z'))))
      renameBoundWithout Set.empty 'x' expr' `shouldBe` L 'y' (Ap (L 'a' (V 'a')) (V 'x'))

  describe "boundNames" $
    it "should return all bound names" $ do
      boundNames (L 'x' (Ap (V 'y') (V 'x'))) `shouldBe` Set.singleton 'x'
      boundNames (L 'x' (L 'y' (V 'x'))) `shouldBe` Set.singleton 'x'
      boundNames (V 'y') `shouldBe` Set.empty

  describe "freeNames" $
    it "should return all free names" $ do
       freeNames (L 'x' (Ap (V 'y') (V 'x'))) `shouldBe` Set.singleton 'y'
       freeNames (V 'y') `shouldBe` Set.singleton 'y'
       freeNames (Ap (V 'y') (V 'x')) `shouldBe` Set.fromList ['y', 'x']

  describe "names" $
    it "should return full list of all names, free or bound" $ do
       names (L 'x' (Ap (V 'y') (V 'x'))) `shouldBe` Set.fromList "yx"
       names (V 'y') `shouldBe` Set.singleton 'y'
       names (Ap (V 'y') (V 'x')) `shouldBe` Set.fromList "yx"

  describe "substitute" $ do
    it "should substitute a free variable in an expression" $ do
      let expr = L 'x' (Ap (V 'y') (V 'x'))
      substitute 'y' (V 'z') expr `shouldBe` L 'x' (Ap (V 'z') (V 'x'))

    it "should re-name variables in by using the same logic as renameBound" $ do
      let expr = L 'x' (Ap (V 'a') (V 'x'))
      substitute 'a' (V 'x') expr `shouldBe` L 'b' (Ap (V 'x') (V 'b'))

  describe "eval" $ do
    it "should .. evaluate things!" $ do
      eval (Ap (L 'x' (Ap (V 'y') (V 'x'))) (V 'a')) `shouldBe` Right (Ap (V 'y') (V 'a'))
      eval (L 'x' (Ap (V 'y') (V 'x'))) `shouldBe` Right (L 'x' (Ap (V 'y') (V 'x')))
      eval (V 'x') `shouldBe` Right (V 'x')
      eval (Ap combI combI) `shouldBe` Right combI
      eval (Ap (Ap combK (V 'y')) (V 'x')) `shouldBe` Right (V 'y')

    it "should fail if it tries to apply a variable" $
      eval (Ap (V 'x') (V 'y')) `shouldBe` Left ("cannot apply " ++ show (V 'y') ++ " to variable (" ++ show (V 'x') ++ ")")

  describe "evalSteps" $ do
    it "should should show no steps in valuating to itself" $ do
      evalSteps (L 'x' (V 'y'))`shouldBe` Right []
      evalSteps (V 'y') `shouldBe` Right []

    it "should show evaluation steps in plain english" $ do
      evalSteps (Ap (L 'x' (V 'x')) (V 'y')) `shouldBe` Right ["Substituting y in place of x in (λx.x), resulting in y"]
      evalSteps (Ap (Ap (L 'x' (L 'y' (V 'x'))) (V 'z')) (V 'a'))
        `shouldBe` Right ["Substituting z in place of x in (λx.(λy.x)), resulting in (λy.z)",
                          "Substituting a in place of y in (λy.z), resulting in z"]
