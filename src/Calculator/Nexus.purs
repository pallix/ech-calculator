module Calculator.Nexus where

import Prelude
import Control.Monad.Reader
import Calculator.Model (Entry(..), Options(..), Process(..), State(..), SystemParams(..), SystemScale, SystemState(..), TimeserieWrapper(..), binning, composting_EatingBinningWormComposting, eating, eating_EatingBinningWormCompostingFoodSharing, foodGardening_EatingBinningWormCompostingFoodGardening, foodGardening_EatingBinningWormCompostingFoodGardeningRainwater, foodSharing, managingWaste, rainwaterCollecting_EatingBinningWormCompostingFoodGardenRainwater, scaleQty)
import Calculator.Rwh (cleaning, collectingWastewater, raining, harvestingRainwaterWithOpenedTank, irrigating)
import Data.Array (drop, foldl, scanl, uncons, (:))
import Data.Date (Date)
import Data.Map (insert)
import Data.Maybe (Maybe(..))
import Rain as Rain
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
                                                                        , harvestingRainwaterWithOpenedTank
                                                                        , collectingWastewater]
      RainwaterHarvestingDemand -> foldl (runProcess sys interval) state' [ raining
                                                                          , harvestingRainwaterWithOpenedTank
                                                                          , cleaning
                                                                          , irrigatingGarden
                                                                          , collectingWastewater]
      _ -> State []


runProcess sys interval st process = runReader (process interval) $ SystemState $ sys { state = st }


scanNexus :: SystemState -> Array SystemState
scanNexus systemState@(SystemState sys@{ scale: {window, period}
                                       , timeseries: ts
                                       , processParams: { rainingParam: { timeserieKey } }} ) =
  scanl nexusSystem systemState' ivals
  where systemState' = SystemState $ sys { timeseries = timeseries' }
        ivals = (intervals window period)
        timeseries' = insert Raining (RainingTimeserie (Rain.buildTimeserie timeserieKey ivals)) ts
        -- TODO supply other timeseries (irrigation, cleaning)
