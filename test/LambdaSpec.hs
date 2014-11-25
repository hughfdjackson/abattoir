module LambdaSpec (specs) where

import Test.Hspec
import Test.QuickCheck
import Lambda
import Data.Set as Set
import Data.Either (Either(..))

possibleNames :: Gen Char
possibleNames = elements ['a'..'z']

instance Arbitrary Expr where
  arbitrary = do
    n <- choose (0, 2) :: Gen Int
    case n of
      0 -> do name <- possibleNames
              return $ v name
              
      1 -> do name <- possibleNames
              expr <- arbitrary
              return $ l name expr

      2 -> do expr  <- arbitrary
              expr' <- arbitrary
              name  <- possibleNames
              return $ ap (l name expr) expr'
  
specs :: Spec
specs = describe "Lambda" $ do
  describe "showExpr" $ do 
    it "should show vars" $ do
      showExpr (v 'x') `shouldBe` "x"

    it "should show appliation" $ do
      showExpr (ap (v 'x') (v 'y')) `shouldBe` "xy"

    it "should show Abstraction" $ do
      showExpr (l 'x' (v 'y')) `shouldBe` "(λx.y)"

  describe "isReducedForm" $ do
    it "should tell whether a form is fulled reduced" $ do
      isReducedForm (ap (l 'x' (v 'x')) (v 'y')) `shouldBe` False
      isReducedForm (l 'x' (ap (v 'x') (v 'y'))) `shouldBe` True
      isReducedForm (v 'x')                      `shouldBe` True

  describe "renameBoundTo" $ do
    it "should rename to the next alphabetical letter *not* bound in the current expression" $ do
      let xToA =  'x' `renameBoundTo` 'a'
      let expr  = l 'x' (l 'y' (l 'z' (ap (ap (v 'x') (v 'y')) (v 'z'))))
            
      xToA expr `shouldBe` l 'a' (l 'y' (l 'z' (ap (ap (v 'a') (v 'y')) (v 'z'))))

    it "It should not rename a free variable" $ do
      let xToA =  'x' `renameBoundTo` 'a'
      let expr = l 'y' (ap (l 'x' (v 'x'))  (v 'x'))

      xToA expr `shouldBe` l 'y' (ap (l 'a' (v 'a')) (v 'x'))

  describe "renameBoundWithout" $ do
    it "should renameBoundTo the first available name in the alphabet given an empty blacklist" $ do 
      let expr  = l 'x' (l 'y' (l 'z' (ap (ap (v 'x') (v 'y')) (v 'z'))))
      let expr' = l 'y' (ap (l 'x' (v 'x'))  (v 'x'))

      renameBoundWithout Set.empty 'x' expr  `shouldBe` l 'a' (l 'y' (l 'z' (ap (ap (v 'a') (v 'y')) (v 'z'))))
      renameBoundWithout Set.empty 'x' expr' `shouldBe` l 'y' (ap (l 'a' (v 'a')) (v 'x'))

  describe "boundNames" $ do
    it "should return all bound names" $ do
      boundNames (l 'x' (ap (v 'y') (v 'x'))) `shouldBe` Set.singleton 'x'
      boundNames (l 'x' (l 'y' (v 'x'))) `shouldBe` Set.singleton 'x'
      boundNames (v 'y') `shouldBe` Set.empty

  describe "freeNames" $ do
    it "should return all free names" $ do
       freeNames (l 'x' (ap (v 'y') (v 'x'))) `shouldBe` Set.singleton 'y'
       freeNames (v 'y') `shouldBe` Set.singleton 'y'
       freeNames (ap (v 'y') (v 'x')) `shouldBe` Set.fromList ['y', 'x']

  describe "names" $ do
    it "should return full list of all names, free or bound" $ do
       names (l 'x' (ap (v 'y') (v 'x'))) `shouldBe` Set.fromList ['y', 'x']
       names (v 'y') `shouldBe` Set.singleton 'y'
       names (ap (v 'y') (v 'x')) `shouldBe` Set.fromList ['y', 'x']
      
  describe "substitute" $ do
    it "should substitute a free variable in an expression" $ do
      let expr = l 'x' (ap (v 'y') (v 'x'))
      substitute 'y' (v 'z') expr `shouldBe` l 'x' (ap (v 'z') (v 'x'))

    it "should re-name variables in by using the same logic as renameBound" $ do
      let expr = l 'x' (ap (v 'a') (v 'x'))
      substitute 'a' (v 'x') expr `shouldBe` l 'b' (ap (v 'x') (v 'b'))

  describe "evaluate" $ do
    it "should .. evaluate things!" $ do 
      eval (ap (l 'x' (ap (v 'y') (v 'x'))) (v 'a')) `shouldBe` Right (ap (v 'y') (v 'a'))
      eval (l 'x' (ap (v 'y') (v 'x'))) `shouldBe` Right (l 'x' (ap (v 'y') (v 'x')))
      eval (v 'x') `shouldBe` Right (v 'x')
      eval (ap combI combI) `shouldBe` Right combI
      eval (ap (ap combK (v 'y')) (v 'x')) `shouldBe` Right (v 'y')

    it "should fail if it tries to apply a variable" $ do 
      eval (ap (v 'x') (v 'y')) `shouldBe` Left ("cannot apply " ++ show (v 'y') ++ " to variable (" ++ show (v 'x') ++ ")")
