import Test.Hspec
import LambdaSpec as Lambda
import SynonymsSpec as Synonyms
import ParseSpec as Parse
import CommandsSpec as Commands

main :: IO ()
main = hspec $
  Lambda.specs >>
  Synonyms.specs >>
  Parse.spec >>
  Commands.spec

  
