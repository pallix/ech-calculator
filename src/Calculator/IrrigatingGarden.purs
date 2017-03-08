module Calculator.IrrigatingGarden where

import Calculator.Timeserie (Timeserie)
import Data.Array (elem)
import Data.Date (Month(..), diff, month)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Days, toDuration)
import Prelude (($), (*), (/))
import Time (TimeInterval(..), TimePeriod(..), nextMonth)

buildTimeserie :: String -> Array TimeInterval -> Timeserie Number
buildTimeserie key intervals = \(TimeInterval { date, period }) ->
  let m = month date
      demandPerDay =
        -- Re the irrigation, this varies according to the
        -- type of soils and plants. UK Royal Horticultural
        -- Society (RHS) suggests that for a square meter of
        -- garden, the average water requirement is 24L for a
        -- period of 7-10 days. Using this as a base and
        -- considering the climate in the UK, we can assume
        -- that in Winter month (Dec, Jan, Feb) - no
        -- irrigation needed, Autumn and Spring (Mar-May, &
        -- Sept-Nov) - 24L per 7 days, and Summer (Jun-Aug) -
        -- 24L per 3.5 days.
        if m `elem` [March, April, May, September, November] then
          24.0 / 7.0
        else
          if m `elem` [June, July, August] then
            24.0 / 3.5
          else
            0.0
  in
    case period of
      OneDay -> Just demandPerDay
      OneMonth -> let nbDays :: Days
                      nbDays = toDuration $ diff (nextMonth date) date
                  in Just $ demandPerDay * (unwrap nbDays)
