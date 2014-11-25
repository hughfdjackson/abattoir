import Test.Hspec
import LambdaSpec as Lambda
import ParseSpec as Parse

main :: IO ()
main = hspec $
  Lambda.specs >>
  Parse.spec

  
