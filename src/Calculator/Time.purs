module Time where

import Data.Date.Component
import Data.Generic
import Data.DateTime as DT
import Data.Date (Date, canonicalDate, day, month, year)
import Data.Enum (fromEnum, succ, toEnum)
import Data.Eq (class Eq)
import Data.Maybe (fromJust, maybe, Maybe(..))
import Data.Ord (class Ord)
import Data.Show (class Show)
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (unfoldr)
import Partial.Unsafe (unsafePartial)
import Prelude (bottom, id, map, show, ($), (<<<), (<=), (<>))

data TimeWindow = TimeWindow { start :: Date
                             , end :: Date }

derive instance genericTimeWindow :: Generic TimeWindow

instance showTimeWindow :: Show TimeWindow  where
    show = gShow

data TimePeriod = OneDay | OneMonth -- OneWeek, OneYear

derive instance genericTimePeriod :: Generic TimePeriod

instance showTimePeriod :: Show TimePeriod  where
    show = gShow

data TimeInterval = TimeInterval { date :: Date
                                 , period :: TimePeriod
                                 }
derive instance genericTimeInterval :: Generic TimeInterval

instance showTimeInterval :: Show TimeInterval  where
    show (TimeInterval { date, period }) = "Ti " <> (show <<< fromEnum <<< year $ date) <> "-" <> (show <<< fromEnum <<< month $ date) <> "-" <> (show <<< fromEnum <<< day $ date)

instance eqTimeInterval :: Eq TimeInterval where
  eq = gEq

instance ordTimeInterval :: Ord TimeInterval where
  compare = gCompare


defaultTi = TimeInterval { date: bottom
                         , period: OneDay
                         }


intervals :: TimeWindow -> TimePeriod -> Array TimeInterval
intervals tw tp = map (TimeInterval <<< { period: tp, date: _ }) (dates tw tp)

-- TODO 'snap' timewindow to fit period
dates :: TimeWindow -> TimePeriod -> Array Date
dates (TimeWindow {start, end}) period =
  unfoldr (\date -> if date <= end then
                      Just (Tuple date (case period of
                                          OneDay -> tomorrow date
                                          OneMonth -> nextMonth date))
                    else Nothing) start


tomorrow :: DT.Date -> DT.Date
tomorrow dt = maybe dt DT.date $ DT.adjust (Days 1.0) (DT.DateTime dt bottom)

nextMonth :: Date -> Date
nextMonth dt = canonicalDate nextY nm.nextM d
  where y = year dt
        m = month dt
        d = day dt
        nm = maybe {nextM: January, incYear: true} {nextM: _, incYear: false} $ succ m
        nextY = if nm.incYear then maybe y id $ succ y else y
