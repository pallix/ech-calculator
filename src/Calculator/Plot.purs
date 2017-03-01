module Calculator.Plot where

import Calculator.Model (Matter(..), MatterProperty(..), Process(..), SystemState(..), TimeserieWrapper(..), foldState)
import Control.Monad (bind, pure)
import Data.Array (foldMap, foldl, head, zip)
import Data.Date (day, month, year)
import Data.Enum (fromEnum, toEnum)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Tuple (Tuple(..))
import Prelude (show, ($), (<<<), (<>))
import Time (TimeInterval(..), intervals)

plotData :: Array SystemState -> String
plotData systemStates =
  foldl plotInterval header (zip (getIntervals $ head systemStates) systemStates)
  where
    getIntervals Nothing = []
    getIntervals (Just (SystemState { scale: {period, window}})) = intervals window period
    header = "# Date Ts-Rain Rainwaterharvested\n"
    plotInterval :: String -> Tuple TimeInterval SystemState -> String
    plotInterval output (Tuple ti@(TimeInterval { date })
                         (SystemState { state, timeseries })) =
      let dateStr = (show <<< fromEnum <<< year $ date) <> "-" <> (show <<< fromEnum <<< month $ date) <> "-" <> (show <<< fromEnum <<< day $ date)
          -- raining = foldState Raining Water GreyWater state
          waterVolumePerSquareCm = fromMaybe 0.0 $ do
            tsw <- lookup Raining timeseries
            case tsw of RainingTimeserie ts -> ts ti
                        _ -> Nothing
          rainwaterHarvesting = foldState RainwaterHarvesting Water GreyWater state
      in
       output <> dateStr <>  " " <> show waterVolumePerSquareCm <> " " <> show rainwaterHarvesting <> "\n"

-- writeData :: String
