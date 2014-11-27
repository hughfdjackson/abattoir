module CommandsSpec (spec) where

import Test.Hspec
import Commands (parse)
import Lambda

spec = describe "Commands" $
  describe "parse" $ do
