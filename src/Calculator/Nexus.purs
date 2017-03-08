module Calculator.Nexus where

import Prelude
import Control.Monad.Reader
import Calculator.Cleaning as Cleaning
import Calculator.IrrigatingGarden as IrrigatingGarden
import Rain as Rain
import Calculator.Model (Entry(..), Matter(..), MatterProperty(..), Options(..), Process(..), Quantity, Quantity(..), State(..), SystemParams(..), SystemScale, SystemState(..), TimeserieWrapper(..), binning, composting_EatingBinningWormComposting, eating, eating_EatingBinningWormCompostingFoodSharing, foldState, foodGardening_EatingBinningWormCompostingFoodGardening, foodGardening_EatingBinningWormCompostingFoodGardeningRainwater, foodSharing, managingWaste, rainwaterCollecting_EatingBinningWormCompostingFoodGardenRainwater, scaleQty)
import Calculator.Rwh (cleaning, irrigatingGarden, pumping, raining, roofCollectingRainwater, tank_collection, tank_demand, wastewaterCollecting)
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
                                 e@(Notification _) -> e : xs
                                 e@(Trace _) -> e : xs

nexusSystem :: SystemState -> TimeInterval -> SystemState
nexusSystem (SystemState sys@{ current, scale, state, systemParams, processParams: processParams } ) interval =
  SystemState $ sys { state = endState }
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
                                                                          , irrigatingGarden
                                                                          , wastewaterCollecting]
      RainwaterHarvestingCollection -> foldl (runProcess sys interval) state' [ raining
                                                                              , roofCollectingRainwater
                                                                              , tank_collection
                                                                              , cleaning
                                                                              , irrigatingGarden
                                                                              , wastewaterCollecting]
      RainwaterHarvestingDistribution -> foldl (runProcess sys interval) state' [ raining
                                                                                , roofCollectingRainwater
                                                                                , tank_collection
                                                                                , pumping
                                                                                -- , distributing
                                                                                , cleaning
                                                                                , irrigatingGarden
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
        -- timeseries' = insert Raining (RainingTimeserie (Rain.buildTimeserie timeserieKey ivals)) ts
        timeseries' = foldr (\(Tuple k t) m -> insert k t m) ts [ Tuple Raining (RainingTimeserie (Rain.buildTimeserie timeserieKey ivals))
                                                                , Tuple Cleaning (CleaningTimeserie (Cleaning.buildTimeserie timeserieKey ivals))
                                                                , Tuple IrrigatingGarden (IrrigatingGardenTimeserie (IrrigatingGarden.buildTimeserie timeserieKey ivals))
                                                                ]
        -- TODO supply other timeseries (irrigation)

type FinalVolumes = { interval :: TimeInterval
                    , volumes :: Map Process (Quantity Matter) }

calculateFinalVolumes :: Array SystemState -> Array FinalVolumes
calculateFinalVolumes systemStates =
  foldr (\systemState arr ->
          let calcVolumes :: SystemState -> Map Process (Quantity Matter)
              -- maybe use a string for the key type here if we need to get different information from a same process
              calcVolumes (SystemState { state }) = fromFoldable [ Tuple Raining                 (foldState Raining                 Water GreyWater  state)
                                                                 , Tuple RoofRainwaterCollecting (foldState RoofRainwaterCollecting Water GreyWater  state)
                                                                 , Tuple Cleaning                (foldState Cleaning                Water BlackWater state)
                                                                 , Tuple TankRainwaterStoring    (foldState TankRainwaterStoring    Water GreyWater  state)
                                                                 , Tuple IrrigatingGarden        (foldState IrrigatingGarden        Water GreyWater  state)
                                                                 , Tuple TapWaterSupplying       (foldState TapWaterSupplying       Water TapWater   state)
                                                                 ]
              calcFinalVolumes ss@(SystemState { interval }) = { interval: interval
                                                               , volumes: calcVolumes ss
                                             }
          in
           cons (calcFinalVolumes systemState) arr) [] systemStates
