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
  -- 7% of total water usage is for outdoors activities (garden, cleaning, water plays)buildTimeserie key intervals = flip lookup $ (foldr feed empty intervals)
  -- we infer 3% is for cleaning
  where usagePerDay = 150.0 * 0.03
