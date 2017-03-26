module Calculator.Cleaning where

import Calculator.Timeserie (Timeserie)
import Data.Date (diff)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Days, toDuration)
import Prelude (($), (*))
import Time (TimeInterval(..), TimePeriod(..), nextMonth)

buildTimeserie :: String -> Array TimeInterval -> Timeserie Number
buildTimeserie key intervals = \(TimeInterval { date, period }) -> case period of
  OneDay -> Just usagePerDay
  OneMonth -> let nbDays :: Days
                  nbDays = toDuration $ diff (nextMonth date) date
              in Just $ usagePerDay * (unwrap nbDays)
  -- UK Energy saving trust did a survey on water use at home and it stated that 1% of the daily water use (150L) is for car washing. The main use of water outdoor is to wash car and tools, in addition to gardening and water play. So we can assume that for the demand of water for outdoor cleaning is 1.5L per person per day. Meakin Estate has 123 flats and UK average household has 2.3 persons. In such way we could work out the daily demand is 1.5*123*2.3 = ~425L
  where usagePerDay = 425.0
