import Test.Hspec
import LambdaSpec as Lambda
import ParseSpec as Parse
import CommandsSpec as Commands

main :: IO ()
main = hspec $
  Lambda.specs >>
  Parse.spec >>
  Commands.spec

  
