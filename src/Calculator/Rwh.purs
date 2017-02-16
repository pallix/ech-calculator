-- Rainwater harvesting
module Rwh
       ( TimeWindow(..)
       , TimeResolution(..)
       , dates
       , tomorrow
       , tw -- example
       )
       where

import Data.Date.Component
import Data.DateTime as DT
import Data.Time.Duration as Duration
import Data.Array (catMaybes, range)
import Data.Date (Date, year, month, day, diff, canonicalDate)
import Data.Enum (fromEnum, toEnum, succ)
import Data.Int (round)
import Data.Map (Map, fromFoldable)
import Data.Maybe (fromJust, maybe, Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (unfoldr)
import Partial.Unsafe (unsafePartial)
import Prelude (bottom, ($), (+), (/), (/=), (<<<), (<=), (==), (>))

data TimeWindow = TimeWindow { start :: Date
                             , end :: Date }

data TimeResolution = OneDay | OneMonth


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

dates :: TimeWindow -> TimeResolution -> Array Date
dates (TimeWindow {start, end}) OneDay =
  unfoldr (\date -> if date <= end then
                      Just (Tuple date (tomorrow date))
                    else Nothing) start
dates _ _ = undefined


tomorrow :: DT.Date -> DT.Date
tomorrow dt = maybe dt DT.date $ DT.adjust (Days 1.0) (DT.DateTime dt bottom)


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

energyNeeded :: Installation -> Number
energyNeeded param = 33.9

type TimeSerie a = Map Month a

rainfallData2012 :: TimeSerie Number
rainfallData2012 = fromFoldable [ Tuple January 1.0
                                , Tuple February 1.0
                                , Tuple March 1.0
                                , Tuple April 1.0
                                , Tuple May 1.0
                                , Tuple June 1.0
                                , Tuple July 1.0
                                , Tuple August 1.0
                                , Tuple September 1.0
                                , Tuple October 1.0
                                , Tuple November 1.0
                                , Tuple December 1.0
                                ]

rainfallData2000Drought :: TimeSerie Number
rainfallData2000Drought = fromFoldable [ Tuple January 1.0
                                       , Tuple February 1.0
                                       , Tuple March 1.0
                                       , Tuple April 1.0
                                       , Tuple May 1.0
                                       , Tuple June 1.0
                                       , Tuple July 1.0
                                       , Tuple August 1.0
                                       , Tuple September 1.0
                                       , Tuple October 1.0
                                       , Tuple November 1.0
                                       , Tuple December 1.0
                                       ]


rainfallData :: { year2012 :: TimeSerie Number
                , year0000Drought :: TimeSerie Number
                }
rainfallData = { year2012 : rainfallData2012
               , year0000Drought : rainfallData2000Drought
               }

type Installation = { tank :: Tank
                    , pump :: Pump
                    , irrigationPoints :: Array { height :: Number
                                                , flowRate :: Number
                                                }
                    , roofSurfaceArea :: Number
                    , roofRunOff :: Number
                    }

rwhParam :: { installations :: Array Installation
            }
rwhParam = { installations : []
           }

type RwhOutput = { energy :: Number
                 }

raining :: forall r. ProcessParam (
          location :: RainwaterLocation | r) -> SystemScale -> Date -> State -> State
raining { location } systemScale@{window, resolution} date state@(State entries) =
  State $
  entries <>
  [ Entry {process: Raining, matter: Water, matterProperty: GreyWater, quantity: rainingWater}
  ]
  where
    timeSeries = timeSeriesbyLocation location
    rainingWater = timeSeries date

tank :: forall r. ProcessParam (
          surfaceArea :: SurfaceArea,
          collectingCapacity :: Number | r) -> SystemScale -> Date -> State -> State
tank {surfaceArea, collectingCapacity} systemScale@{window, resolution} state@(State entries) =
  State $
  entries <>
  [ Entry {process: Raining, matter: Water, matterProperty: GreyWater, quantity: negQty collectedWater}
  , Entry {process: RainwaterCollecting, matter: Water, matterProperty: GreyWater, quantity: collectedWater}
  ]
  where
    rainingWater = foldState Raining Water AllMatterProperty state
    surfaceArea' = blockToRoofSurface surfaceArea
    collectedWater = Volume Water $ surfaceArea' * (scaleNumberOnTime systemScale collectingCapacity)

-- scanl :: ( State -> Date -> State  ) -> InitState -> [ Date ] -> [ States ]

scanNexus :: SystemScale -> SystemState -> [ SystemState ]
scanNexus { window, resolution } = scanl nexusSystem ( dates window resolution )

nexusSystem :: SystemState -> Date -> SystemState
nexusSystem (SystemState sys@{ current, scale@{window, resolution}, state, systemParams, processParams: processParams } ) = SystemState $ sys { state = endState }
  where
    state' = scaleFirstEntry scale systemParams state
    endState = case current of
      EatingOnly -> managingWaste processParams.managedWasteParam
                  $ eating processParams.eatingParam state'
      EatingBinning -> managingWaste processParams.managedWasteParam
                     $ binning processParams.binningParam
                     $ eating processParams.eatingParam state'
      EatingBinningWormComposting -> managingWaste processParams.managedWasteParam
                                   $ binning processParams.binningParam
                                   $ composting_EatingBinningWormComposting processParams.wormCompostingParam scale
                                   $ eating processParams.eatingParam state'
      EatingBinningWormCompostingFoodGardening -> managingWaste processParams.managedWasteParam
                                   $ binning processParams.binningParam
                                   $ foodGardening_EatingBinningWormCompostingFoodGardening processParams.foodGardeningParam scale
                                   $ composting_EatingBinningWormComposting processParams.wormCompostingParam scale
                                   $ eating processParams.eatingParam state'
      EatingBinningWormCompostingFoodGardenWatering -> managingWaste processParams.managedWasteParam
                                   $ binning processParams.binningParam
                                   $ foodGardening_EatingBinningWormCompostingFoodGardening processParams.foodGardeningParam scale
                                   $ composting_EatingBinningWormComposting processParams.wormCompostingParam scale
                                   $ eating processParams.eatingParam state'
      EatingBinningWormCompostingFoodGardenRainwater -> managingWaste processParams.managedWasteParam
                                   $ binning processParams.binningParam
                                   $ foodGardening_EatingBinningWormCompostingFoodGardeningRainwater processParams.foodGardeningParam scale
                                   $ rainwaterCollecting_EatingBinningWormCompostingFoodGardenRainwater processParams.rainwaterCollectingParam scale
                                   $ composting_EatingBinningWormComposting processParams.wormCompostingParam scale
                                   $ eating processParams.eatingParam state'
      EatingBinningFoodSharing -> managingWaste processParams.managedWasteParam
                                 $ binning processParams.binningParam
                                 -- TODO replug on eating?
                                 -- $ eating ...
                                 $ foodSharing processParams.foodSharingParam
                                 $ eating_EatingBinningWormCompostingFoodSharing processParams.eatingParam state'
      EatingBinningWormCompostingFoodSharing -> managingWaste processParams.managedWasteParam
                                   $ binning processParams.binningParam
                                   $ composting_EatingBinningWormComposting processParams.wormCompostingParam scale
                                   -- TODO replug on eating?
                                   -- $ eating ...
                                   $ foodSharing processParams.foodSharingParam
                                   $ eating_EatingBinningWormCompostingFoodSharing processParams.eatingParam state'

      _ -> State []

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
