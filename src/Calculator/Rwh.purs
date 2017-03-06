-- Rainwater harvesting
module Calculator.Rwh where

import Data.Date.Component
import Calculator.Model (Entry(Notification, Entry, Trace), Matter(..), MatterProperty(..), NotificationType(..), Options(..), Process(..), Quantity(ZeroQuantity, Volume), Scale(..), State(State), SurfaceArea(SurfaceArea), SystemParams(SystemParams), SystemState(SystemState), Time(..), TimeserieWrapper(..), addQty, cappedQty, foldState, initProcessParams, negQty, subQty, blockToRoofSurface)
import Control.Monad (bind, pure)
import Control.Monad.Reader (Reader, ask)
import Data.Array (index)
import Data.Date (Date, day, month)
import Data.Enum (fromEnum)
import Data.Map (Map, empty, lookup)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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


raining ::
     TimeInterval
  -> Reader SystemState State
raining ti = do
    SystemState { state: (State entries)
                 , systemParams: (SystemParams { estateSurfaceArea })
                 , timeseries
                 } <- ask
    let waterVolumePerSquareCm = fromMaybe 0.0 $ do
          tsw <- lookup Raining timeseries
          case tsw of RainingTimeserie ts -> ts ti
                      _ -> Nothing
          -- TODO: more unit convertion to do here
        rainingWater = Volume Water $ (case estateSurfaceArea of (SurfaceArea sa) -> sa * waterVolumePerSquareCm)
    pure $ State $ entries <>
      [ Entry {process: Raining, matter: Water, matterProperty: GreyWater, quantity: rainingWater}
      ]

harvestingRainwaterWithOpenedTank ::
     TimeInterval
  -> Reader SystemState State
harvestingRainwaterWithOpenedTank ti = do
  SystemState { state: state@(State entries)
              , processParams: { storingRainwaterParam: { surfaceArea
                                                        , capacity
                                                        }
                               }
              , timeseries
              } <- ask
  let waterVolumePerSquareCm = fromMaybe 0.0 $ do
          tsw <- lookup Raining timeseries
          case tsw of RainingTimeserie ts -> ts ti
                      _ -> Nothing
      -- TODO: eventually more unit convertion to do here later
      harvestableVolume = Volume Water $ (case surfaceArea of (SurfaceArea sa) -> sa * waterVolumePerSquareCm)
      volumeInTank = foldState StoringRainwater Water GreyWater state
      freeVolumeInTank = subQty capacity volumeInTank
      harvestedVolume = cappedQty harvestableVolume freeVolumeInTank
      overflow = subQty harvestableVolume harvestedVolume
      entries' = [ Entry { process: Raining
                         , matter: Water
                         , matterProperty: GreyWater
                         , quantity: negQty harvestableVolume
                         }
                 , Entry { process: StoringRainwater,
                           matter: Water,
                           matterProperty: GreyWater,
                           quantity: harvestedVolume
                         }
                 ] <> if overflow > ZeroQuantity then
                        [ Entry { process: StoringRainwater
                                , matter: Waste
                                , matterProperty: GreyWater
                                , quantity: overflow
                                } ]
                        else []
      notifications = [Notification { process: StoringRainwater
                                    , typ: TankOverflow
                                    , on: overflow > ZeroQuantity} ]
      traces = [ Trace { process: StoringRainwater
                       , message: " freevolumeintank " <> show freeVolumeInTank }
               , Trace { process: StoringRainwater
                       , message: " capacity " <> show capacity }
               , Trace { process: StoringRainwater
                       , message: " volumeintank " <> show volumeInTank }
               ]
  pure $ State $ entries <> traces <> entries' <> notifications


collectingRainwater ::
     TimeInterval
  -> Reader SystemState State
collectingRainwater ti = do
  SystemState { state: state@(State entries)
              , processParams: { rainwaterCollectingParam: { surfaceArea
                                                           , collectingCapacity }  }
              , timeseries
              } <- ask
  let waterVolumePerSquareCm = fromMaybe 0.0 $ do
          tsw <- lookup Raining timeseries
          case tsw of RainingTimeserie ts -> ts ti
                      _ -> Nothing
      surfaceArea' = blockToRoofSurface surfaceArea
      -- TODO recheck unit convertion here
      collectedWater = Volume Water $ surfaceArea' * (waterVolumePerSquareCm * 10000.0) * collectingCapacity
      traces = [ Trace { process: RainwaterCollecting
                       , message: " waterVolumePerSquareCm " <> show waterVolumePerSquareCm }
               , Trace { process: RainwaterCollecting
                       , message: " surfacearea' " <> show surfaceArea' }
               , Trace { process: RainwaterCollecting
                       , message: " collectingcapacity " <> show collectingCapacity }
               ]
      entries' = [ Entry { process: Raining
                         , matter: Water
                         , matterProperty: GreyWater
                         , quantity: negQty collectedWater}
                 , Entry { process: RainwaterCollecting
                         , matter: Water
                         , matterProperty: GreyWater
                         , quantity: collectedWater}
                 ]
  pure $ State $ entries <> traces <> entries'

storingRainwaterInTank ::
     TimeInterval
  -> Reader SystemState State
storingRainwaterInTank ti = do
  SystemState { state: state@(State entries)
              , processParams: { storingRainwaterParam: { capacity
                                                           }
                               }
              , timeseries
              } <- ask
  let harvestableVolume = foldState  RainwaterCollecting Water GreyWater state
      volumeInTank = foldState StoringRainwater Water GreyWater state
      freeVolumeInTank = subQty capacity volumeInTank
      harvestedVolume = cappedQty harvestableVolume freeVolumeInTank
      overflow = subQty harvestableVolume harvestedVolume
      entries' = [ Entry { process: Raining
                         , matter: Water
                         , matterProperty: GreyWater
                         , quantity: negQty harvestableVolume
                         }
                 , Entry { process: StoringRainwater,
                           matter: Water,
                           matterProperty: GreyWater,
                           quantity: harvestedVolume
                         }
                 ] <> if overflow > ZeroQuantity then
                        [ Entry { process: StoringRainwater
                                , matter: Waste
                                , matterProperty: GreyWater
                                , quantity: overflow
                                } ]
                        else []
      notifications = [Notification { process: StoringRainwater
                                    , typ: TankOverflow
                                    , on: overflow > ZeroQuantity} ]
      traces = [ Trace { process: StoringRainwater
                       , message: " freevolumeintank " <> show freeVolumeInTank }
               , Trace { process: StoringRainwater
                       , message: " capacity " <> show capacity }
               , Trace { process: StoringRainwater
                       , message: " volumeintank " <> show volumeInTank }
               ]
  pure $ State $ entries <> traces <> entries' <> notifications




collectingWastewater ::
  TimeInterval
  -> Reader SystemState State
collectingWastewater _ = do
  SystemState { state: state@(State entries)
              } <- ask
  let wasteWaterRwh = foldState StoringRainwater Waste GreyWater state
      wasteWaterCleaning = foldState Cleaning Waste BlackWater state
  pure $ State $ entries <>
    [ Entry { process: WastewaterCollecting
            , matter: Waste, matterProperty: BlackWater
            , quantity: addQty wasteWaterCleaning wasteWaterRwh }
    ]

cleaning ::
     TimeInterval
  -> Reader SystemState State
cleaning ti = do
    SystemState { state: state@(State entries)
                 , processParams: { cleaningParam: { surfaceArea
                                                   , waterConsumptionPerSqm }}
                 } <- ask
    let waterNeeded = Volume Water $ (unwrap surfaceArea) * waterConsumptionPerSqm
        -- TODO use a timeserie here
        volumeInTank = foldState StoringRainwater Water GreyWater state
        tankWaterConsumed = cappedQty waterNeeded volumeInTank
        tapWaterConsumed = subQty waterNeeded tankWaterConsumed
        entries' = [ Entry { process: StoringRainwater
                           , matter: Water
                           , matterProperty: GreyWater
                           , quantity: negQty tankWaterConsumed }
                   , Entry { process: TapWaterSupplying
                           , matter: Water
                           , matterProperty: TapWater
                           , quantity: negQty tapWaterConsumed }
                   , Entry { process: Cleaning
                           , matter: Waste
                           , matterProperty: BlackWater
                           , quantity: waterNeeded }
                   ]
        notifications = [Notification { process: TapWaterSupplying
                                      , typ: CleaningNotEnoughTankWater
                                      , on: tapWaterConsumed > ZeroQuantity } ]
    pure $ State $ entries <> entries' <> notifications

irrigatingGarden ::
     TimeInterval
  -> Reader SystemState State
irrigatingGarden ti = do
    SystemState { state: state@(State entries)
                , timeseries
                 } <- ask
    let waterNeeded = Volume Water $ fromMaybe 0.0 $ do
          tsw <- lookup IrrigatingGarden timeseries
          case tsw of IrrigationGardenTimeserie ts -> ts ti
                      _ -> Nothing
        -- TODO implement timeserie here
        volumeInTank = foldState StoringRainwater Water GreyWater state
        tankWaterConsumed = cappedQty waterNeeded volumeInTank
        tapWaterConsumed = subQty waterNeeded tankWaterConsumed
        traces = [ Trace { process: IrrigatingGarden
                         , message: " tapWaterConsumed " <> show tapWaterConsumed }
                 ]
        entries' = [ Entry { process: StoringRainwater
                           , matter: Water
                           , matterProperty: GreyWater
                           , quantity: negQty tankWaterConsumed }
                   , Entry { process: TapWaterSupplying
                           , matter: Water
                           , matterProperty: TapWater
                           , quantity: negQty tapWaterConsumed }
                   , Entry { process: IrrigatingGarden
                           , matter: Waste
                           , matterProperty: BlackWater
                           , quantity: addQty tankWaterConsumed tapWaterConsumed }
                   ]
        notifications = [Notification { process: TapWaterSupplying
                                      , typ: IrrigationGardenNotEnoughTankWater
                                      , on: tapWaterConsumed > ZeroQuantity } ]
    pure $ State $ entries <> traces <> entries' <> notifications


-- TODO: see how to do more the calculation in a more DSL style

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
