module Calculator.Nexus where

import Prelude
import Control.Monad.Reader
import Calculator.Cleaning as Cleaning
import Calculator.IrrigatingGarden as IrrigatingGarden
import Rain as Rain
import Calculator.Model (Entry(..), Matter, Matter(..), MatterProperty(..), Options(..), Process(..), Quantity, Quantity(..), State(..), SystemParams(..), SystemScale, SystemState(..), TimeserieWrapper(..), binning, composting_EatingBinningWormComposting, eating, eating_EatingBinningWormCompostingFoodSharing, foldState, foodGardening_EatingBinningWormCompostingFoodGardening, foodGardening_EatingBinningWormCompostingFoodGardeningRainwater, foodSharing, initialState, managingWaste, rainwaterCollecting_EatingBinningWormCompostingFoodGardenRainwater, scaleQty, subQty)
import Calculator.Rwh (cleaning, cleaning_distribution, irrigatingGarden_demand, irrigatingGarden_distribution, pumping, raining, roofCollectingRainwater, tank_collection, tank_demand, wastewaterCollecting)
import Calculator.Timeserie (Timeserie)
import Data.Array (cons, drop, foldl, foldr, scanl, uncons, (:))
import Data.Date (Date)
import Data.Map (Map, empty, fromFoldable, insert)
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
                                                                                , wastewaterCollecting]
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

type VolumesInfo = { interval :: TimeInterval
                   , timeseries :: Map Process TimeserieWrapper
                   , volumes :: { initialRainwater :: Quantity Matter
                                 , tankStoredRainwater :: Quantity Matter
                                 , overflowTank :: Quantity Matter
                                 , irrigatingGardenWater :: Quantity Matter
                                 , tapWaterUsed :: Quantity Matter } }

showVolumesInfo { interval,
                  volumes: { initialRainwater
                           , tankStoredRainwater
                           , overflowTank
                           , tapWaterUsed }
                } = show interval <> " " <> show initialRainwater <> " " <> show tankStoredRainwater <> " " <> show overflowTank <> " " <> show tapWaterUsed

calculateVolumesInfo :: Array SystemState -> Array VolumesInfo
calculateVolumesInfo systemStates =
  foldr (\systemState arr ->
          let calcVolumes (SystemState { state }) = { initialRainwater:           (foldState    Raining                 Water GreyWater  state)
                                                    , tankStoredRainwater:        (foldState    TankRainwaterStoring    Water GreyWater  state)
                                                    , overflowTank:               (foldState    WastewaterCollecting    Waste Overflow   state)
                                                    , tapWaterUsed:               (initialState TapWaterSupplying       Waste TapWater   state) `subQty` (foldState TapWaterSupplying       Waste TapWater   state)
                                                    , irrigatingGardenWater:      (foldState    IrrigatingGarden        Waste BlackWater state)
                                                      -- TODO add others stuff here
                                                    }
              calcFinalVolumes ss@(SystemState { interval, timeseries }) = { interval
                                                                           , timeseries
                                                                           , volumes: calcVolumes ss
                                             }
          in
           cons (calcFinalVolumes systemState) arr) [] systemStates
