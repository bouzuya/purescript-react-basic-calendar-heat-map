module Test.Main
  ( main
  ) where

import Effect (Effect)
import Prelude (Unit, discard)
import Test.Format as Format
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Test.WeekDate as WeekDate

main :: Effect Unit
main = runTest do
  suite "Main" do
    test "1 == 1" do
      Assert.equal 1 1
  Format.tests
  WeekDate.tests
