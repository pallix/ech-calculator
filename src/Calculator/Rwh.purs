-- Rainwater harvesting
module Rwh where

import Data.Date.Component
import Data.DateTime as DT
import Data.Time.Duration as Duration
import Calculator.Model (Entry(..), Matter(..), MatterProperty(..), Process(..), ProcessParam, Quantity(..), State(..), SurfaceArea(..), SystemParams(..), SystemScale, SystemState(..))
import Control.Monad (bind, pure)
import Control.Monad.Reader (Reader, ask)
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

raining ::
     Date
  -> Reader SystemState State
raining date = do
    (SystemState { state: (State entries)
                 , processParams: { rainingParam: { rainfallDataKey
                                                  , rainfallData }}
                 , systemParams: (SystemParams { estateSurfaceArea })
                 , scale: { resolution: resolution }
                 }) <- ask
    let timeSerie = maybe empty id $ lookup rainfallDataKey rainfallData
        monthData = maybe [] id $ lookup (month date) timeSerie
        waterVolumePerSquareCm = case resolution of
          OneDay -> maybe 0.0 id $ index monthData (fromEnum <<< day $ date)
          OneMonth -> sum monthData
        rainingWater = Volume Water $ (case estateSurfaceArea of (SurfaceArea sa) -> sa * waterVolumePerSquareCm)
    pure $ State $ entries <>
      [ Entry {process: Raining, matter: Water, matterProperty: GreyWater, quantity: rainingWater}
      ]

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
