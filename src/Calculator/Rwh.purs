-- Rainwater harvesting
module Rwh
       ( TimeWindow(..)
       , TimeResolution(..)
       , toRange
       )
       where

import Prelude
import Data.Date (Date, diff, canonicalDate)
import Data.Date.Component
import Data.Time.Duration as Duration
import Data.Enum (toEnum)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Data.Newtype (unwrap)
import Data.Array (range)
import Data.Int (round)

data TimeWindow = TimeWindow { start :: Date
                             , end :: Date }

data TimeResolution = OneDay | OneMonth


-- dateStart = unsafePartial $ canonicalDate (fromJust $ toEnum 2017) January (fromJust $ toEnum 1)
-- dateEnd = unsafePartial $ canonicalDate (fromJust $ toEnum 2017) March (fromJust $ toEnum 30)

toRange :: TimeWindow -> TimeResolution -> Array Int
toRange (TimeWindow tw) tr = range 0 (round <<< (_ / nbIntervals) <<< unwrap $ duration)
  where
    duration :: Duration.Days
    duration = Duration.toDuration $ diff tw.end tw.start
    nbIntervals = case tr of
      OneMonth -> 30.0
      OneDay -> 1.0
