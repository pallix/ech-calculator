module Calculator.Plot where

import Calculator.Model (Matter(..), MatterProperty(..), Process(..), SystemState(..), foldState)
import Data.Array (foldMap, foldl, head, zip)
import Data.Date (day, month, year)
import Data.Enum (fromEnum, toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Prelude (show, ($), (<<<), (<>))
import Time (TimeInterval(..), intervals)


plotData :: Array SystemState -> String
plotData systemStates =
  foldl plotInterval header (zip (getIntervals $ head systemStates) systemStates)
  where
    getIntervals Nothing = []
    getIntervals (Just (SystemState {scale: {period, window}})) = intervals window period
    header = "# Date Rainwater \n"
    plotInterval :: String -> Tuple TimeInterval SystemState -> String
    plotInterval output (Tuple (TimeInterval { date }) (SystemState { state })) =
      let dateStr = (show <<< fromEnum <<< year $ date) <> "-" <> (show <<< fromEnum <<< month $ date) <> "-" <> (show <<< fromEnum <<< day $ date)
          raining = foldState Raining Water GreyWater state
      in
       output <> dateStr <>  " " <> show raining <> "\n"

-- writeData :: String
