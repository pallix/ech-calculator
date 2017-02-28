module Rain where

import Data.Date.Component
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(Tuple))


type TimeSerie = Map Month (Array Number)

rainfallData2012 :: TimeSerie
rainfallData2012 = fromFoldable [ Tuple January [3.0, 2.9]
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

rainfallData2013 :: TimeSerie
rainfallData2013 = fromFoldable [ Tuple January [1.0]
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

-- probably (Array Number) if rain timeseries have a resolution per day
type RainfallData = Map String TimeSerie


rainfallData :: Map String (Map Month (Array Number))
rainfallData = fromFoldable [ Tuple "2012" rainfallData2012,
                              Tuple "2013" rainfallData2013 ]
