import Test.Hspec
import LambdaSpec as Lambda
import LambdaWithSynonymsSpec as LambdaWithSynonyms
import ParseSpec as Parse
import CommandsSpec as Commands

main :: IO ()
main = hspec $
  Lambda.specs >>
  Parse.spec >>
  LambdaWithSynonyms.spec >>
  Commands.spec

  
