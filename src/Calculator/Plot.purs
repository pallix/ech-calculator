module Calculator.Plot where

import Node.ChildProcess
import Control.Monad.Eff.Exception
import Calculator.Model (Matter(..), MatterProperty(..), Process(..), SystemState(..), TimeserieWrapper(..), foldState, showQ)
import Calculator.Nexus (FoldedState)
import Control.Monad (bind, pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (foldMap, foldl, head, zip)
import Data.Date (day, month, year)
import Data.Enum (fromEnum, toEnum)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (writeTextFile)
import Prelude (show, ($), (*), (<<<), (<>))
import Time (TimeInterval(..), intervals)

toGnuPlotFormat :: Array FoldedState -> String
toGnuPlotFormat foldedStates =
  foldl plotInterval header foldedStates
  where
    header = "# Date Ts-Rain Tank Overflow Garden Collected Pump Tap Cleaning\n"
    plotInterval :: String -> FoldedState -> String
    plotInterval output { interval: ti@(TimeInterval {date})
                        , timeseries
                        , volumes: { tankStoredRainwater
                                   , overflowTank
                                   , irrigatingGardenWater
                                   , roofRainwaterCollected
                                   , pumpStoredRainwater
                                   , tapWaterUsed
                                   , cleaningWaterUsed
                                     }} =
      let dateStr = (show <<< fromEnum <<< year $ date) <> "-" <> (show <<< fromEnum <<< month $ date) <> "-" <> (show <<< fromEnum <<< day $ date)
          watermm = fromMaybe 0.0 $ do
            tsw <- lookup Raining timeseries
            case tsw of RainingTimeserie ts -> ts ti
                        _ -> Nothing
      in
       output <> dateStr <>  " " <>
       show (watermm * 100.0) <> " " <> -- 100.0 is arbitrary, just for plotting nicely
       showQ tankStoredRainwater <> " " <>
       showQ overflowTank <> " " <>
       showQ irrigatingGardenWater <> " " <>
       showQ roofRainwaterCollected <> " " <>
       showQ pumpStoredRainwater <> " " <>
       showQ tapWaterUsed <> " " <>
       showQ cleaningWaterUsed <> " " <>
       -- if you add something here, add its name to the header variable above
       -- and edit ~/scripts/plot.gp
       "\n"


plotData foldedStates = do
  writeTextFile UTF8 "/tmp/nexus.dat" content
--  fork "./scripts/plot.pg" []
  process <- spawn "./scripts/plot.gp" [""] defaultSpawnOptions
  onError process errorHandler
  log ""
  where
    content = toGnuPlotFormat foldedStates
    errorHandler e = throwException $ toStandardError e
