module Format
  ( dayOfWeekShortName
  ) where

import Data.Date as Date
import Data.String as String
import Prelude (show, (<<<))

dayOfWeekShortName :: Date.Weekday -> String
dayOfWeekShortName = String.take 3 <<< show
