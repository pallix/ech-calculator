-- Rainwater harvesting
module Rwh where

import Data.Date.Component
import Data.DateTime as DT
import Data.Time.Duration as Duration
import Calculator.Model (Entry(..), Matter(..), MatterProperty(..), Process(..), ProcessParam, Quantity(..), State(..), SurfaceArea(..), SystemParams(..), SystemScale, SystemState(..))
import Data.Array (catMaybes, index, range)
import Data.Date (Date, canonicalDate, day, diff, month, year)
import Data.Enum (fromEnum, succ, toEnum)
import Data.Int (round)
import Data.Map (Map, empty, fromFoldable, lookup)
import Data.Maybe (fromJust, maybe, Maybe(..))
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Time.Duration (Days(..))
import Data.Traversable (sum)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (unfoldr)
import Partial.Unsafe (unsafePartial)
import Prelude (bottom, id, ($), (*), (+), (/), (/=), (<<<), (<=), (==), (>))
import Time (TimeResolution(..))

type RainfallTimeseries = Map Month Number

-- todo: efficiency, living time
-- noize?
-- https://www.rainwaterharvesting.org.au/rainwater-harvesting-maintenance-advice/pumps-and-switching-devices
-- http://www.ajdesigner.com/phppump/pump_equations_water_horse_power.php

data Pump = LowQuality | HighQuality

type Tank = { size :: Number
            , waterButtHeight :: Number
            , evaporation :: Number
            }

-- energyNeeded :: Installation -> Number
-- energyNeeded param = 33.9

type TimeSerie = Map Month (Array Number)

rainfallData2012 :: TimeSerie
rainfallData2012 = fromFoldable [ Tuple January [1.0]
                                , Tuple February [1.0]
                                , Tuple March [1.0]
                                , Tuple April [1.0]
                                , Tuple May [1.0]
                                , Tuple June [1.0]
                                , Tuple July [1.0]
                                , Tuple August [1.0]
                                , Tuple September [1.0]
                                , Tuple October [1.0]
                                , Tuple November [1.0]
                                , Tuple December [1.0]
                                ]

rainfallData2013 :: TimeSerie
rainfallData2013 = fromFoldable [ Tuple January [1.0]
                                , Tuple February [1.0]
                                , Tuple March [1.0]
                                , Tuple April [1.0]
                                , Tuple May [1.0]
                                , Tuple June [1.0]
                                , Tuple July [1.0]
                                , Tuple August [1.0]
                                , Tuple September [1.0]
                                , Tuple October [1.0]
                                , Tuple November [1.0]
                                , Tuple December [1.0]
                                ]

-- probably (Array Number) if rain timeseries have a resolution per day
type RainfallData = Map String TimeSerie


rainfallData = fromFoldable [ Tuple "2012" rainfallData2012,
                              Tuple "2013" rainfallData2013 ]

-- type Installation = { tank :: Tank
--                     , pump :: Pump
--                     , irrigationPoints :: Array { height :: Number
--                                                 , flowRate :: Number
--                                                 }
--                     , roofSurfaceArea :: Number
--                     , roofRunOff :: Number
--                     }

-- rwhParam :: { installations :: Array Installation
--             }
-- rwhParam = { installations : []
--            }

type RwhOutput = { energy :: Number
                 }


tankParam :: { size :: Int
             , waterButtHeight :: Int
             , evaporation :: Int
             }
tankParam = { size: 10 -- liters
            , waterButtHeight: 0
            , evaporation: 0
            }

rainingParam = { title: "Raining"
               , rainfallDataKey: "2012"
               , rainfallData: rainfallData}

raining
  :: forall r.
     SystemParams
  -> ProcessParam( rainfallDataKey :: String
                 , rainfallData :: RainfallData | r)
  -> SystemScale
  -> Date
  -> State
  -> State
raining (SystemParams {estateSurfaceArea}) {rainfallDataKey , rainfallData} systemScale@{resolution} date state@(State entries) =
  State $
  entries <>
  [ Entry {process: Raining, matter: Water, matterProperty: GreyWater, quantity: rainingWater}
  ]
  where
    timeSerie = maybe empty id $ lookup rainfallDataKey rainfallData
    monthData = maybe [] id $ lookup (month date) timeSerie
    waterVolumePerSquareCm = case resolution of
      OneDay -> maybe 0.0 id $ index monthData (fromEnum <<< day $ date)
      OneMonth -> sum monthData
    rainingWater = Volume Water $ waterVolumePerSquareCm * (case estateSurfaceArea of (SurfaceArea sa) -> sa * waterVolumePerSquareCm)

-- 1. Time series rainfall data
-- 2. Roof area
-- 3. Roof runoff coefficient  roof pitch types
-- 4. Water loss
-- 5. Tank (size, position, number of tank, material used)
-- 6. Distribution ( length of the pipes and material used)
-- 7. Pumping energy (energy consumption, energy source, number of pumps)
-- 8. First flush device (http://www.urbanfoodgarden.org/main/water-management/calculating--roof-runoff.htm)
-- 9. Pollution / Filtering

-- todo: Pumping energy is determined by the flowrate and the head
-- (i.e. different between tank and irrigation point)

-- Retention and throttle
-- Additional storage is provided within the storage tank (a “retention” volume), which is released at a low flow rate via a throttle valve to sewers, decreasing incidents of sewer flooding.

-- http://www.harvesth2o.com/rainwater_harvesting_UK.shtml
