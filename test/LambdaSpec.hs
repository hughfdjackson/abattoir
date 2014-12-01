module LambdaSpec (specs) where

import           Data.Either     ()
import           Data.Set        as Set
import           Data.Map        as Map
import           Lambda
import           Test.Hspec
import qualified Combinators

specs :: Spec
specs = describe "Lambda" $ do
  describe "Expr show" $ do
    it "should show vars" $
      show (V 'x') `shouldBe` "x"

    it "should show application" $
      show (Ap (V 'x') (V 'y')) `shouldBe` "xy"

    it "should clarify right application with parens" $
      show (Ap (V 'x') (Ap (V 'y') (V 'x'))) `shouldBe` "x(yx)"

    it "should show Abstraction" $
      show (L 'x' (V 'y')) `shouldBe` "(λx.y)"

    it "should show nested functions as a function of 'multiple arguments'" $ do
      show (L 'x' (L 'y' (V 'y'))) `shouldBe` "(λxy.y)"
      show (L 'x' (L 'y' (L 'z' (V 'z')))) `shouldBe` "(λxyz.z)"

  describe "renameBoundTo" $ do
    it "should rename to the next alphabetical letter *not* bound in the current expression" $ do
      let xToA =  'x' `renameBoundTo` 'a'
      let expr  = L 'x' (L 'y' (L 'z' (Ap (Ap (V 'x') (V 'y')) (V 'z'))))

      xToA expr `shouldBe` L 'a' (L 'y' (L 'z' (Ap (Ap (V 'a') (V 'y')) (V 'z'))))

    it "should not rename a free variable" $ do
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
      eval (L 'x' (Ap (V 'y') (V 'x'))) `shouldBe` L 'x' (Ap (V 'y') (V 'x'))
      eval (V 'x') `shouldBe` V 'x'
      eval (Ap Combinators.i Combinators.i) `shouldBe` Combinators.i
      eval (Ap (Ap Combinators.k (V 'y')) (V 'x')) `shouldBe` V 'y'

    it "should evaluate a returned application until no more applications exist" $
      eval (Ap (L 'y' (Ap (V 'y') (V 'x'))) Combinators.i) `shouldBe` V 'x'

    it "should evaluate resolvable applications *within* lambda bodies until resolved" $
      eval (L 'y' (Ap (L 'x' (V 'x')) (V 'y'))) `shouldBe` L 'y' (V 'y')

    it "should stop evaluating if it tries to apply a variable" $ do
      eval (Ap (V 'x') (V 'y')) `shouldBe` Ap (V 'x') (V 'y')
      eval (Ap (L 'x' (Ap (V 'y') (V 'x'))) (V 'a')) `shouldBe` Ap (V 'y') (V 'a')

    it "should evaluate S0 to one" $
      eval (Ap Combinators.s Combinators.zero) `shouldBe` L 'y' (L 'x' (Ap (V 'y') (V 'x')))

    it "should evaluate SS0 to two" $
      eval (Ap Combinators.s (Ap Combinators.s Combinators.zero)) `shouldBe` L 'y' (L 'x' (Ap (V 'y') (Ap (V 'y') (V 'x'))))

    it "should evaluate S to itself" $
      eval Combinators.s `shouldBe` Combinators.s

  describe "evalSteps" $ do
    it "should should show no steps in valuating to itself" $ do
      evalSteps (L 'x' (V 'y'))`shouldBe` []
      evalSteps (V 'y') `shouldBe` []

    it "should show evaluation steps in plain english" $ do
      evalSteps (Ap (L 'x' (V 'x')) (V 'y')) `shouldBe` ["(λx.x)y == [y/x] x == y"]
      evalSteps (Ap (Ap (L 'x' (L 'y' (V 'x'))) (V 'z')) (V 'a'))
        `shouldBe` ["(λxy.x)z == [z/x] (λy.x) == (λy.z)",
                    "(λy.z)a == [a/y] z == z"]
