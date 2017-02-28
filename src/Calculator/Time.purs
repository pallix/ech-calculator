module Time where

import Data.Date.Component
import Data.Generic
import Data.DateTime as DT
import Data.Date (Date, canonicalDate, day, month, year)
import Data.Enum (succ, toEnum)
import Data.Maybe (fromJust, maybe, Maybe(..))
import Data.Show (class Show)
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (unfoldr)
import Partial.Unsafe (unsafePartial)
import Prelude (bottom, id, ($), (<=))

data TimeWindow = TimeWindow { start :: Date
                             , end :: Date }

derive instance genericTimeWindow :: Generic TimeWindow

instance showTimeWindow :: Show TimeWindow  where
    show = gShow

data TimeResolution = OneDay | OneMonth -- OneWeek, OneYear

derive instance genericTimeResolution :: Generic TimeResolution

instance showTimeResolution :: Show TimeResolution  where
    show = gShow

dateStart = unsafePartial $ canonicalDate (fromJust $ toEnum 2017) January (fromJust $ toEnum 1)
dateEnd = unsafePartial $ canonicalDate (fromJust $ toEnum 2017) March (fromJust $ toEnum 30)
tw = TimeWindow { start : dateStart, end: dateEnd }

-- toRange :: TimeWindow -> TimeResolution -> Array Int
-- toRange (TimeWindow tw) tr = range 0 (round <<< (_ / nbIntervals) <<< unwrap $ duration)
--   where
--     duration :: Duration.Days
--     duration = Duration.toDuration $ diff tw.end tw.start
--     nbIntervals = case tr of
--       OneMonth -> 30.0
--       OneDay -> 1.0


-- TODO 'snap' timewindow to fit resolution
dates :: TimeWindow -> TimeResolution -> Array Date
dates (TimeWindow {start, end}) resolution =
  unfoldr (\date -> if date <= end then
                      Just (Tuple date (case resolution of
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
