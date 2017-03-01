-- Rainwater harvesting
module Calculator.Rwh where

import Data.Date.Component
import Calculator.Model (Entry(Notification, Entry, Trace), Matter(..), MatterProperty(..), NotificationType(..), Options(..), Process(..), Quantity(ZeroQuantity, Volume), Scale(..), State(State), SurfaceArea(SurfaceArea), SystemParams(SystemParams), SystemState(SystemState), Time(..), addQty, cappedQty, foldState, initProcessParams, negQty, subQty)
import Control.Monad (bind, pure)
import Control.Monad.Reader (Reader, ask)
import Data.Array (index)
import Data.Date (Date, day, month)
import Data.Enum (fromEnum)
import Data.Map (Map, empty, lookup)
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Traversable (sum)
import Prelude (id, show, ($), (*), (-), (<), (<<<), (>))
import Time (TimeInterval(..), TimePeriod(..))

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

openedTank :: { surfaceArea :: SurfaceArea }
openedTank = { surfaceArea : SurfaceArea 10.0
             }

raining ::
     TimeInterval
  -> Reader SystemState State
raining (TimeInterval ti) = do
    SystemState { state: (State entries)
                 , processParams: { rainingParam: { rainfallDataKey
                                                  , rainfallData }}
                 , systemParams: (SystemParams { estateSurfaceArea })
                 } <- ask
    let timeSerie = maybe empty id $ lookup rainfallDataKey rainfallData
        monthData = maybe [] id $ lookup (month ti.date) timeSerie
        waterVolumePerSquareCm = case ti.period of
          OneDay -> maybe 0.0 id $ index monthData ((_ - 1) <<< fromEnum <<< day $ ti.date)
          OneMonth -> sum monthData
          -- TODO: eventually more unit convertion to do here later
        rainingWater = Volume Water $ (case estateSurfaceArea of (SurfaceArea sa) -> sa * waterVolumePerSquareCm)
    pure $ State $ entries <>
      [ Entry {process: Raining, matter: Water, matterProperty: GreyWater, quantity: rainingWater}
      ]


-- TODO clean Notification
rainwaterHarvesting_tank ::
     TimeInterval
  -> Reader SystemState State
rainwaterHarvesting_tank (TimeInterval ti) = do
  SystemState { state: state@(State entries)
              , processParams: { rainingParam: { rainfallDataKey
                                               , rainfallData }
                               , rainwaterHarvestingParam: { surfaceArea
                                                           , capacity
                                                           }
                               }
              } <- ask
  let timeSerie = fromMaybe empty $ lookup rainfallDataKey rainfallData
      monthData = fromMaybe [] $ lookup (month ti.date) timeSerie
      waterVolumePerSquareCm = case ti.period of
        OneDay -> fromMaybe 0.0 $ index monthData ((_ - 1) <<< fromEnum <<< day $ ti.date)
        OneMonth -> sum monthData
      -- TODO: eventually more unit convertion to do here later
      harvestableVolume = Volume Water $ (case surfaceArea of (SurfaceArea sa) -> sa * waterVolumePerSquareCm)
      volumeInTank = foldState RainwaterHarvesting Water GreyWater state
      freeVolumeInTank = subQty capacity volumeInTank
      harvestedVolume = cappedQty harvestableVolume freeVolumeInTank
      overflow = subQty harvestableVolume harvestedVolume
      entries' = [ Entry { process: Raining
                         , matter: Water
                         , matterProperty: GreyWater
                         , quantity: negQty harvestableVolume
                         }
                 , Entry { process: RainwaterHarvesting,
                           matter: Water,
                           matterProperty: GreyWater,
                           quantity: harvestedVolume
                         }
                 ] <> if overflow > ZeroQuantity then
                        [ Entry { process: RainwaterHarvesting,
                                  matter: Waste,
                                  matterProperty: GreyWater,
                                  quantity: overflow
                                } ]
                        else []
      notifications = if overflow > ZeroQuantity then
                        [Notification { process: RainwaterHarvesting
                                        -- type
                                      , typ: RainwaterHarvestingWaterOverflow } ]
                      else []
      traces = [ Trace { process: RainwaterHarvesting
                       , message: " freevolumeintank " <> show freeVolumeInTank }
               , Trace { process: RainwaterHarvesting
                       , message: " capacity " <> show capacity }
               , Trace { process: RainwaterHarvesting
                       , message: " volumeintank " <> show volumeInTank }
               ]
  pure $ State $ entries <> traces <> entries' <> notifications


collectingWastewater ::
  TimeInterval
  -> Reader SystemState State
collectingWastewater _ = do
  SystemState { state: state@(State entries)
                 , processParams: { rainingParam: { rainfallDataKey
                                                  , rainfallData }}
                 , systemParams: (SystemParams { estateSurfaceArea })
                 , scale: { period }
                 } <- ask
  let wasteWaterRwh = foldState RainwaterHarvesting Waste GreyWater state
      wasteWaterCleaning = foldState Cleaning Waste BlackWater state
  pure $ State $ entries <>
    [ Entry { process: WastewaterCollecting
            , matter: Waste, matterProperty: BlackWater
            , quantity: addQty wasteWaterCleaning wasteWaterRwh }
    ]

-- TODO: cleaning do not occur every day
-- TODO: mix with tap water?
cleaning ::
     TimeInterval
  -> Reader SystemState State
cleaning date = do
    SystemState { state: state@(State entries)
                 , processParams: { cleaningParam: { surfaceArea
                                                   , waterConsumptionPerSqm }}
                 , scale: { period }
                 } <- ask
    let waterNeeded = Volume Water $ (unwrap surfaceArea) * waterConsumptionPerSqm
        volumeInTank = foldState RainwaterHarvesting Water GreyWater state
        waterConsumed = cappedQty waterNeeded volumeInTank
        entries' = [ Entry { process: RainwaterHarvesting
                           , matter: Water
                           , matterProperty: GreyWater
                           , quantity: negQty waterConsumed }
                     -- waste or dark water?
                   , Entry { process: Cleaning
                           , matter: Waste
                           , matterProperty: BlackWater
                           , quantity: waterConsumed }
                   ]
        notifications = if (waterConsumed < waterNeeded) then
                          [Notification { process: Cleaning
                                        , typ: CleaningNotEnoughTankWater } ]
                      else []
    pure $ State $ entries <> entries' <> notifications

-- TODO: see how to do more the calculation in a more DSL style
-- TODO: have a simpler data model to express various timeseries

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
