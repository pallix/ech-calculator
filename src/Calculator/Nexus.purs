module Calculator.Nexus where

import Prelude
import Control.Monad.Reader
import Calculator.Model (Entry(..), Options(..), State(..), SystemParams(..), SystemScale, SystemState(..), binning, composting_EatingBinningWormComposting, eating, eating_EatingBinningWormCompostingFoodSharing, foodGardening_EatingBinningWormCompostingFoodGardening, foodGardening_EatingBinningWormCompostingFoodGardeningRainwater, foodSharing, managingWaste, rainwaterCollecting_EatingBinningWormCompostingFoodGardenRainwater, scaleQty)
import Calculator.Rwh (cleaning, raining, rainwaterHarvesting_tank)
import Data.Array (foldl, scanl, uncons, (:))
import Data.Date (Date)
import Data.Maybe (Maybe(..))
import Time (dates)

scaleFirstEntry :: SystemScale -> SystemParams -> State -> State
scaleFirstEntry systemScale systemParams (State entries) =
  State $ case uncons entries of
    Nothing -> []
    Just {head: h,
          tail: xs} -> case h of Entry entry@{quantity} -> (Entry $ entry { quantity = scaleQty systemScale systemParams quantity }) : xs
                                 e@(Notification _) -> e : xs

nexusSystem :: SystemState -> Date -> SystemState
nexusSystem (SystemState sys@{ current, scale, state, systemParams, processParams: processParams } ) date =
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
      RainwaterHarvestingTank -> foldl (runProcess sys date) state' [raining, rainwaterHarvesting_tank]
      RainwaterHarvestingDemand -> foldl (runProcess sys date) state' [raining, rainwaterHarvesting_tank, cleaning]
      _ -> State []

runProcess sys date state process = runReader (process date) $ SystemState $ sys { state = state }

scanNexus :: SystemState -> Array SystemState
scanNexus systemState@(SystemState { scale: {resolution, window} } ) =
  scanl nexusSystem systemState (dates window resolution)
