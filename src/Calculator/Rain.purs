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


-- TODO plug real data and select on `key` parameter
buildTimeserie :: String -> Array TimeInterval -> Timeserie Number
buildTimeserie key intervals = flip lookup $ (foldr feed empty intervals)
  where
    feed :: TimeInterval -> Map TimeInterval Number -> Map TimeInterval Number
    feed ti@(TimeInterval { date, period }) m =
      case period of
        OneDay -> insert ti (fromMaybe 0.0 $ index monthData ((_ - 1) <<< fromEnum <<< day $ date)) m
        OneMonth -> insert ti (sum monthData) m
        where
          monthData = fromMaybe [] $ lookup (month date) selected
    selected :: Map Month (Array Number)
    selected = fromMaybe empty $ lookup key rainfalls
    -- Daily rainfall in NW3 weather station in 2013, unit:mm, London
    rainfalls :: Map String (Map Month (Array Number))
    rainfalls = fromFoldable
                [Tuple "2013" (fromFoldable [ Tuple January [1.5, 1.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 2.9, 0.0, 0.0, 0.5, 0.0, 4.9, 0.0, 0.0, 0.0, 5.0, 0.1, 8.0, 0.0, 0.0, 0.3, 0.0, 0.3, 4.1, 7.1, 2.0, 1.5, 5.7, 1.5]
                                            , Tuple February [7.2, 0.0, 0.0, 0.0, 2.0, 0.3, 2.0, 0.5, 0.3, 12.9, 7.6, 0.0, 0.0, 3.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.8, 0.0, 0.1, 0.1, 0.0, 0.0, 0.1, 0.0, 0.0]
                                            , Tuple March [0.1, 0.0, 0.0, 0.0, 0.0, 0.3, 4.6, 12.5, 2.0, 0.0, 0.3, 0.0, 0.3, 0.0, 3.7, 4.6, 3.7, 11.5, 0.0, 0.0, 0.0, 3.4, 6.8, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.0]
                                            , Tuple April [0.0, 0.0, 0.0, 0.6, 0.0, 0.0, 0.0, 0.0, 0.9, 4.3, 4.1, 10.2, 6.5, 0.0, 0.0, 0.0, 0.0, 0.8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.0, 3.9, 0.1, 0.0, 0.0, 0.0]
                                            , Tuple May [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.2, 1.1, 0.0, 5.8, 0.9, 0.3, 8.8, 0.1, 1.1, 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0.1, 9.3, 0.0, 0.0, 0.0, 14.7, 0.5, 5.1, 0.0]
                                            , Tuple June [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0.0, 0.0, 2.0, 0.1, 0.0, 4.2, 0.1, 0.0, 0.0, 0.0, 0.1, 2.7, 3.7, 0.1, 0.0, 0.0, 0.0, 0.7, 1.8, 0.0, 0.0]
                                            , Tuple July [0.0, 0.1, 1.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 12.2, 0.0, 3.9, 0.0, 6.7, 2.4, 1.8, 7.2, 0.6]
                                            , Tuple August [0.0, 2.8, 0.9, 0.0, 10.3, 0.0, 0.0, 0.0, 1.5, 0.0, 0.0, 0.0, 0.0, 0.1, 0.1, 0.7, 0.2, 0.0, 0.0, 0.0, 0.0, 5.2, 0.0, 19.2, 10.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
                                            , Tuple September [0.0, 0.0, 0.0, 0.0, 0.0, 3.3, 0.9, 0.6, 6.8, 0.6, 1.8, 1.0, 32.0, 3.4, 3.0, 0.9, 5.5, 0.3, 0.9, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.0, 0.0]
                                            , Tuple October [0.0, 2.5, 1.2, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 7.6, 4.0, 11.4, 3.7, 0.0, 5.5, 0.0, 0.0, 0.9, 13.8, 0.6, 5.1, 6.7, 0.0, 1.5, 1.5, 8.9, 15.2, 0.0, 0.0, 1.5]
                                            , Tuple November [0.1, 0.3, 11.6, 3.9, 8.5, 3.7, 0.6, 7.9, 3.6, 0.3, 3.9, 2.1, 0.0, 0.3, 0.0, 0.0, 0.6, 1.2, 0.0, 6.1, 0.6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.0, 0.0, 0.0]
                                            , Tuple December [0.0, 0.0, 0.0, 0.0, 1.8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.7, 1.2, 5.5, 8.8, 6.1, 6.1, 4.3, 0.0, 8.1, 0.0, 15.2, 15.2, 3.3, 0.0, 3.7, 0.0, 0.0, 4.8, 4.2]
                                            ])
                ]
