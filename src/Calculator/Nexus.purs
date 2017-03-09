module Calculator.Nexus where

import Prelude
import Control.Monad.Reader
import Calculator.Cleaning as Cleaning
import Calculator.IrrigatingGarden as IrrigatingGarden
import Rain as Rain
import Calculator.Model (Entry(..), Matter, Matter(..), MatterProperty(..), NotificationType, Options(..), Process(..), Quantity, Quantity(..), State(..), SystemParams(..), SystemScale, SystemState(..), TimeserieWrapper(..), binning, composting_EatingBinningWormComposting, eating, eating_EatingBinningWormCompostingFoodSharing, foldFlows, foldNotifications, foldState, foldStateTi, foodGardening_EatingBinningWormCompostingFoodGardening, foodGardening_EatingBinningWormCompostingFoodGardeningRainwater, foodSharing, initialState, initialStateTi, lastState, managingWaste, rainwaterCollecting_EatingBinningWormCompostingFoodGardenRainwater, scaleQty, subQty)
import Calculator.Rwh (cleaning, cleaning_distribution, irrigatingGarden_demand, irrigatingGarden_distribution, pumping, raining, roofCollectingRainwater, tank_collection, tank_demand, tapWaterSupplying, wastewaterCollecting, wastewaterCollecting_distribution)
import Calculator.Timeserie (Timeserie)
import Data.Array (cons, drop, foldl, foldr, scanl, uncons, (:), fromFoldable)
import Data.Date (Date)
import Data.Map (Map, empty, insert, keys)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Time (TimeInterval, dates, intervals)

scaleFirstEntry :: SystemScale -> SystemParams -> State -> State
scaleFirstEntry systemScale systemParams (State entries) =
  State $ case uncons entries of
    Nothing -> []
    Just {head: h,
          tail: xs} -> case h of Entry entry@{quantity} -> (Entry $ entry { quantity = scaleQty systemScale systemParams quantity }) : xs
                                 e@(_) -> e : xs

nexusSystem :: SystemState -> TimeInterval -> SystemState
nexusSystem (SystemState sys@{ current, scale, state, systemParams, processParams: processParams } ) interval =
  SystemState $ sys { state = endState
                    , interval = interval
                    }
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
      --------- below here we use the new design (using a monad reader) to represent processes
      RainwaterHarvestingTank -> foldl (runProcess sys interval) state' [ raining
                                                                        , tank_demand
                                                                        , wastewaterCollecting]
      RainwaterHarvestingDemand -> foldl (runProcess sys interval) state' [ raining
                                                                          , tank_demand
                                                                          , cleaning
                                                                          , irrigatingGarden_demand
                                                                          , wastewaterCollecting]
      RainwaterHarvestingCollection -> foldl (runProcess sys interval) state' [ raining
                                                                              , roofCollectingRainwater
                                                                              , tank_collection
                                                                              , cleaning
                                                                              , irrigatingGarden_demand
                                                                              , wastewaterCollecting]
      RainwaterHarvestingDistribution -> foldl (runProcess sys interval) state' [ raining
                                                                                , roofCollectingRainwater
                                                                                , tank_collection
                                                                                , pumping
                                                                                , irrigatingGarden_distribution
                                                                                , cleaning_distribution
                                                                                , wastewaterCollecting_distribution]
      _ -> State []


runProcess sys interval st process = runReader (process interval) $ SystemState $ sys { state = st
                                                                                      , interval = interval
                                                                                      }


scanNexus :: SystemState -> Array SystemState
scanNexus systemState@(SystemState sys@{ scale: {window, period}
                                       , timeseries: ts
                                       , processParams: { rainingParam: { timeserieKey } }} ) =
  scanl nexusSystem systemState' ivals
  where systemState' = SystemState $ sys { timeseries = timeseries' }
        ivals = intervals window period
        -- TODO some intervals are calculated with a slightly wrong number of days to due to
        -- https://github.com/purescript/purescript-datetime/issues/48
        timeseries' = foldr (\(Tuple k t) m -> insert k t m) ts [ Tuple Raining (RainingTimeserie (Rain.buildTimeserie timeserieKey ivals))
                                                                , Tuple Cleaning (CleaningTimeserie (Cleaning.buildTimeserie timeserieKey ivals))
                                                                , Tuple IrrigatingGarden (IrrigatingGardenTimeserie (IrrigatingGarden.buildTimeserie timeserieKey ivals))
                                                                ]

type FoldedState = { interval :: TimeInterval
                   , timeseries :: Map Process TimeserieWrapper
                   , volumes :: { initialRainwater :: Quantity Matter
                                 , tankStoredRainwater :: Quantity Matter
                                 , overflowTank :: Quantity Matter
                                 , irrigatingGardenWater :: Quantity Matter
                                 , roofRainwaterCollected :: Quantity Matter
                                 , tapWaterUsed :: Quantity Matter
                                 , pumpStoredRainwater :: Quantity Matter
                                 , cleaningWaterUsed :: Quantity Matter
                                 }
                   , notifications :: { tapWaterSupplying :: Array NotificationType
                                      , pumping :: Array NotificationType
                                      , tankRainwaterStoring :: Array NotificationType
                                      }
                   , flows :: { pumping :: Number }
                   }

showFoldedStates { interval,
                   volumes: { initialRainwater
                           , tankStoredRainwater
                           , overflowTank
                           , tapWaterUsed
                           , pumpStoredRainwater
                           }
                } = show interval <> " " <> show initialRainwater <> " " <> show tankStoredRainwater <> " " <> show overflowTank <> " " <> show tapWaterUsed

mapFoldStates :: Array SystemState -> Array FoldedState
mapFoldStates systemStates =
    map (\systemState ->
        let foldVolumes (SystemState { state, interval }) =
              { initialRainwater:           (foldState       Raining                 Water GreyWater  state)
              , tankStoredRainwater:        (foldState       TankRainwaterStoring    Water GreyWater           state)
              , pumpStoredRainwater:        (foldState       Pumping                 Water GreyWater           state)
              , overflowTank:               (foldStateTi     WastewaterCollecting    Waste Overflow   interval state)
              , tapWaterUsed:               (initialStateTi  TapWaterSupplying       Waste TapWater   interval state) `subQty`
                (foldStateTi     TapWaterSupplying       Water TapWater   interval state)
              , irrigatingGardenWater:      (foldStateTi     IrrigatingGarden        Waste Absorbed   interval state)
              , roofRainwaterCollected:     (initialStateTi  RoofRainwaterCollecting Water GreyWater  interval state)
              , cleaningWaterUsed:          (foldStateTi     WastewaterCollecting    Waste BlackWater interval state)
              }
            foldNotifs :: SystemState -> { tapWaterSupplying :: Array NotificationType
                                         , pumping :: Array NotificationType
                                         , tankRainwaterStoring :: Array NotificationType
                                         }
            foldNotifs (SystemState { state }) = { tapWaterSupplying: fromFoldable <<< keys $ foldNotifications TapWaterSupplying state
                                                 , pumping: fromFoldable <<< keys $ foldNotifications Pumping state
                                                 , tankRainwaterStoring: fromFoldable <<< keys $ foldNotifications TankRainwaterStoring state
                                                 }
            foldFlos (SystemState { state }) = { pumping: foldFlows Pumping state }
            foldStates ss@(SystemState { interval, timeseries }) = { interval
                                                                   , timeseries
                                                                   , volumes: foldVolumes ss
                                                                   , notifications: foldNotifs ss
                                                                   , flows: foldFlos ss
                                                                   }
        in
         foldStates systemState) systemStates
