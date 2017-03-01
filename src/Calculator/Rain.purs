module Rain where

import Data.Date.Component
import Calculator.Timeserie (Timeserie)
import Data.Array (foldr, index)
import Data.Date (day, month)
import Data.Enum (fromEnum)
import Data.Foldable (sum)
import Data.Map (Map, empty, fromFoldable, insert, lookup)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Prelude (flip, ($), (-), (<<<))
import Time (TimeInterval(..), TimePeriod(..))

buildTimeserie :: Array TimeInterval -> Timeserie Number
buildTimeserie intervals = flip lookup $ (foldr feed empty intervals)
  where
    feed :: TimeInterval -> Map TimeInterval Number -> Map TimeInterval Number
    feed ti@(TimeInterval { date, period }) m =
      case period of
        OneDay -> insert ti (fromMaybe 0.0 $ index monthData ((_ - 1) <<< fromEnum <<< day $ date)) m
        OneMonth -> insert ti (sum monthData) m
        where
          monthData = fromMaybe [] $ lookup (month date) sample
    sample = fromFoldable [ Tuple January [1.0, 2.0, 0.0]
                          , Tuple February [1.0]
                          , Tuple March [1.0]
                          , Tuple April [1.0]
                          , Tuple May [1.0]
                          , Tuple June [1.0]
                          , Tuple July [1.0]
                          , Tuple August [1.0]
                          , Tuple September [1.0]
                          , Tuple October [1.0]
                          , Tuple November [1.0]
                          , Tuple December [1.0]
                          ]
