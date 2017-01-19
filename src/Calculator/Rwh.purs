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
import Data.Map (Map)

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


type RainfallTimeseries = Map Month Number

-- todo: efficiency, living time
data Pump = LowQuality | HighQuality

type TankParam = { size :: Number
                 , pump :: Pump
                 , elevation :: Number
                 , distanceToIrrigationPoint :: Number }

rwhParam :: Array TankParam
rwhParam = []



-- 1. Time series rainfall data
-- 2. Roof area
-- 3. Roof runoff coefficient  roof pitch types
-- 4. Water loss
-- 5. Tank (size, position, number of tank, material used)
-- 6. Distribution ( length of the pipes and material used)
-- 7. Pumping energy (energy consumption, energy source, number of pumps)

-- todo: influence of the pipes?
-- todo: Pumping energy is determined by the flowrate and the head
-- (i.e. different between tank and irrigation point)

-- Retention and throttle
-- Additional storage is provided within the storage tank (a “retention” volume), which is released at a low flow rate via a throttle valve to sewers, decreasing incidents of sewer flooding.

-- http://www.harvesth2o.com/rainwater_harvesting_UK.shtml
