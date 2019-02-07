module Test.Format
  ( tests
  ) where

import Data.Date (Weekday)
import Data.Enum (enumFromTo)
import Format (dayOfWeekShortName)
import Prelude (bottom, top, (<$>))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Format" do
  test "dayOfWeekShortName" do
    Assert.equal
      (["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"])
      (dayOfWeekShortName <$> enumFromTo bottom top :: Array Weekday)
