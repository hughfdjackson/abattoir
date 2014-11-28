module CommandsSpec (spec) where

import Test.Hspec
import Commands (parse, Command(..))
import Lambda

spec :: SpecWith ()
spec = describe "Commands" $
  describe "parse" $ do
    it "should parse :help to Help" $ do
      parse ":help" `shouldBe` Right Help
      parse ":help   " `shouldBe` Right Help
      parse " :help" `shouldBe` Right Help

    it "should parse :step expr to step" $ do
      parse ":step (\\x.x)" `shouldBe` Right (Step (L 'x' (V 'x')))
      parse ":step    (\\x.x)" `shouldBe` Right (Step (L 'x' (V 'x')))
      parse "  :step (\\x.x)  " `shouldBe` Right (Step (L 'x' (V 'x')))


    it "should parse :quit to quit" $ do
      parse ":quit" `shouldBe` Right Quit
      parse "  :quit  " `shouldBe` Right Quit

    it "should parse expr to expr" $ do
      parse "(\\x.x)" `shouldBe` Right (Eval (L 'x' (V 'x')))
      parse "   (\\x.x)" `shouldBe` Right (Eval (L 'x' (V 'x')))
      parse "(\\x.x)    " `shouldBe` Right (Eval (L 'x' (V 'x')))