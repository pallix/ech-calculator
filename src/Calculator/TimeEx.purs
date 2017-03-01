module Calculator.TimeEx where

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
import Time (TimeWindow(..))

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
