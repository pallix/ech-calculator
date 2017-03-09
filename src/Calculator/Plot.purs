module Calculator.Plot where

import Node.ChildProcess
import Control.Monad.Eff.Exception
import Calculator.Model (Matter(..), MatterProperty(..), Process(..), SystemState(..), TimeserieWrapper(..), foldState, showQ)
import Calculator.Nexus (VolumesInfo)
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

toGnuPlotFormat :: Array VolumesInfo -> String
toGnuPlotFormat systemStates =
  foldl plotInterval header systemStates
  where
    header = "# Date Ts-Rain Tank Overflow Garden Collected Pump\n"
    plotInterval :: String -> VolumesInfo -> String
    plotInterval output { interval: ti@(TimeInterval {date})
                        , timeseries
                        , volumes: { tankStoredRainwater
                                   , overflowTank
                                   , irrigatingGardenWater
                                   , roofRainwaterCollected
                                   , pumpStoredRainwater
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
       showQ pumpStoredRainwater <>
       "\n"


plotData volumesInfo = do
  writeTextFile UTF8 "/tmp/nexus.dat" content
--  fork "./scripts/plot.pg" []
  process <- spawn "./scripts/plot.gp" [""] defaultSpawnOptions
  onError process errorHandler
  log ""
  where
    content = toGnuPlotFormat volumesInfo
    errorHandler e = throwException $ toStandardError e
