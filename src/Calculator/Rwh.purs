-- Rainwater harvesting
module Calculator.Rwh where

import Data.Date.Component
import Calculator.Model (Entry(Notification, Entry, Trace), Matter(..), MatterProperty(..), NotificationType(..), Options(..), Process(..), Quantity(ZeroQuantity, Volume), Scale(..), State(State), SurfaceArea(SurfaceArea), SystemParams(SystemParams), SystemState(SystemState), Time(..), TimeserieWrapper(..), addQty, blockToRoofSurface, cappedQty, foldState, initProcessParams, negQty, subQty)
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
import Debug.Trace (spy)
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
    let rainingmm = fromMaybe 0.0 $ do
          tsw <- lookup Raining timeseries
          case tsw of RainingTimeserie ts -> ts ti
                      _ -> Nothing
          -- TODO: recheck unit convertion here
        rainingWater = Volume Water $ (case estateSurfaceArea of (SurfaceArea sa) -> sa * (rainingmm * 0.001)) * 1000.0
    pure $ State $ entries <>
      [ Entry {process: Raining, matter: Water, matterProperty: GreyWater, quantity: rainingWater}
      ]

tank_demand ::
     TimeInterval
  -> Reader SystemState State
tank_demand ti = do
  SystemState { state: state@(State entries)
              , processParams: { tankRainwaterStoringParam: { surfaceArea
                                                        , capacity
                                                        }
                               }
              , systemParams: SystemParams { estateSurfaceArea }
              , timeseries
              } <- ask
  let rainingmm = fromMaybe 0.0 $ do
          tsw <- lookup Raining timeseries
          case tsw of RainingTimeserie ts -> ts ti
                      _ -> Nothing
      -- TODO: eventually more unit convertion to do here later
      harvestableVolume = Volume Water $ (case estateSurfaceArea of SurfaceArea sa -> sa * rainingmm * 0.001) * 1000.0
      volumeInTank = foldState TankRainwaterStoring Water GreyWater state
      freeVolumeInTank = subQty capacity volumeInTank
      harvestedVolume = cappedQty harvestableVolume freeVolumeInTank
      overflow = subQty harvestableVolume harvestedVolume
      entries' = [ Entry { process: Raining
                         , matter: Water
                         , matterProperty: GreyWater
                         , quantity: negQty harvestableVolume
                         }
                 , Entry { process: TankRainwaterStoring,
                           matter: Water,
                           matterProperty: GreyWater,
                           quantity: harvestedVolume
                         }
                 ] <> if overflow > ZeroQuantity then
                        [ Entry { process: TankRainwaterStoring
                                , matter: Waste
                                , matterProperty: GreyWater -- TODO use matterProperty 'Overflow' for better tracking
                                , quantity: overflow
                                } ]
                        else []
      notifications = [Notification { process: TankRainwaterStoring
                                    , typ: TankOverflow
                                    , on: overflow > ZeroQuantity} ]
      traces = [ Trace { process: TankRainwaterStoring
                       , message: " freevolumeintank " <> show freeVolumeInTank }
               , Trace { process: TankRainwaterStoring
                       , message: " capacity " <> show capacity }
               , Trace { process: TankRainwaterStoring
                       , message: " volumeintank " <> show volumeInTank }
               ]
  pure $ State $ entries <> traces <> entries' <> notifications


roofCollectingRainwater ::
     TimeInterval
  -> Reader SystemState State
roofCollectingRainwater ti = do
  SystemState { state: state@(State entries)
              , processParams: { rainwaterCollectingParam: { numberOfBlocks
                                                           , collectingCapacity }  }
              , timeseries
              } <- ask
  let rainingmm = fromMaybe 0.0 $ do
          tsw <- lookup Raining timeseries
          case tsw of RainingTimeserie ts -> ts ti
                      _ -> Nothing
      surfaceArea' = blockToRoofSurface numberOfBlocks
      -- TODO recheck unit convertion here
      collectedWater = Volume Water $ surfaceArea' * (rainingmm * 0.001) * 1000.0 * collectingCapacity
      traces = [ Trace { process: RoofRainwaterCollecting
                       , message: " rainingmm " <> show rainingmm }
               , Trace { process: RoofRainwaterCollecting
                       , message: " surfacearea' " <> show surfaceArea' }
               , Trace { process: RoofRainwaterCollecting
                       , message: " collectingcapacity " <> show collectingCapacity }
               ]
      entries' = [ Entry { process: Raining
                         , matter: Water
                         , matterProperty: GreyWater
                         , quantity: negQty collectedWater}
                 , Entry { process: RoofRainwaterCollecting
                         , matter: Water
                         , matterProperty: GreyWater
                         , quantity: collectedWater}
                 ]
  pure $ State $ entries <> traces <> entries'


pumping ::
     TimeInterval
  -> Reader SystemState State
pumping ti = do
  SystemState { state: state@(State entries)
              , processParams: { pumpingParam: { suctionHead }
                               , distributingParam: { dischargeHead }}
              } <- ask
  let -- power = head * flow * 1000 * 9.81 / (3.6 * 1000000)
      -- todo: efficiency
      -- http://www.engineeringtoolbox.com/pumps-power-d_505.html
      traces = [ Trace { process: Pumping
                       , message: " hello " }
               ]
      entries' = [
                 ]
  pure $ State $ entries <> traces <> entries'

tank_collection ::
     TimeInterval
  -> Reader SystemState State
tank_collection ti = do
  SystemState { state: state@(State entries)
              , processParams: { tankRainwaterStoringParam: { capacity
                                                           }
                               }
              , timeseries
              } <- ask
  let harvestableVolume = foldState RoofRainwaterCollecting Water GreyWater state
      volumeInTank = foldState TankRainwaterStoring Water GreyWater state
      freeVolumeInTank = subQty capacity volumeInTank
      harvestedVolume = cappedQty harvestableVolume freeVolumeInTank
      overflow = subQty harvestableVolume harvestedVolume
      entries' = [ Entry { process: Raining
                         , matter: Water
                         , matterProperty: GreyWater
                         , quantity: negQty harvestableVolume
                         }
                 , Entry { process: TankRainwaterStoring,
                           matter: Water,
                           matterProperty: GreyWater,
                           quantity: harvestedVolume
                         }
                 ] <> if overflow > ZeroQuantity then
                        [ Entry { process: TankRainwaterStoring
                                , matter: Waste
                                , matterProperty: GreyWater
                                , quantity: overflow
                                } ]
                        else []
      notifications = [Notification { process: TankRainwaterStoring
                                    , typ: TankOverflow
                                    , on: overflow > ZeroQuantity} ]
      traces = [ Trace { process: TankRainwaterStoring
                       , message: " freevolumeintank " <> show freeVolumeInTank }
               , Trace { process: TankRainwaterStoring
                       , message: " capacity " <> show capacity }
               , Trace { process: TankRainwaterStoring
                       , message: " volumeintank " <> show volumeInTank }
               ]
  pure $ State $ entries <> traces <> entries' <> notifications




wastewaterCollecting ::
  TimeInterval
  -> Reader SystemState State
wastewaterCollecting _ = do
  SystemState { state: state@(State entries)
              } <- ask
  let wasteWaterRwh = foldState TankRainwaterStoring Waste GreyWater state
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
                 , timeseries
                 } <- ask
    let waterNeeded = Volume Water $ fromMaybe 0.0 $ do
          tsw <- lookup Cleaning timeseries
          case tsw of CleaningTimeserie ts -> ts ti
                      _ -> Nothing
        volumeInTank = foldState TankRainwaterStoring Water GreyWater state
        tankWaterConsumed = cappedQty waterNeeded volumeInTank
        tapWaterConsumed = subQty waterNeeded tankWaterConsumed
        traces = [ Trace { process: Cleaning
                         , message: " waterNeeded " <> show waterNeeded }
                 ]
        entries' = [ Entry { process: TankRainwaterStoring
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
    pure $ State $ entries <> traces <> entries' <> notifications

irrigatingGarden ::
     TimeInterval
  -> Reader SystemState State
irrigatingGarden ti = do
    SystemState { state: state@(State entries)
                , timeseries
                 } <- ask
    let waterNeeded = Volume Water $ fromMaybe 0.0 $ do
          tsw <- lookup IrrigatingGarden timeseries
          case tsw of IrrigatingGardenTimeserie ts -> ts ti
                      _ -> Nothing
        volumeInTank = foldState TankRainwaterStoring Water GreyWater state
        tankWaterConsumed = cappedQty waterNeeded volumeInTank
        tapWaterConsumed = subQty waterNeeded tankWaterConsumed
        traces = [ Trace { process: IrrigatingGarden
                         , message: " waterNeeded " <> show waterNeeded },
                   Trace { process: IrrigatingGarden
                         , message: " tapWaterConsumed " <> show tapWaterConsumed }
                 ]
        entries' = [ Entry { process: TankRainwaterStoring
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
