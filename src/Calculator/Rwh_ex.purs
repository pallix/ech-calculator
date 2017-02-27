module RwhEx where

import Data.Date.Component
import Control.Monad
import Rwh as R
import Calculator.Model (Options(..), Scale(..), State(..), SurfaceArea(..), SystemParams(..), SystemState(..), Time(..), initProcessParams)
import Control.Monad.Reader (runReader)
import Data.Date (Date, canonicalDate, day, month, year)
import Data.Enum (succ, toEnum)
import Data.Maybe (fromJust, maybe, Maybe(..))
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (unfoldr)
import Partial.Unsafe (unsafePartial)
import Prelude (bottom, id, ($), (<=))
import Time (TimeResolution(..))


systemParamsEx = SystemParams { houseHoldSize: 199
                              , estatePopulation : 200
                              , estateAveragePersonPerHousehold : 2.4
                              , estateFlatsOneBedroom : 70
                              , estateFlatsTwoBedroom : 23
                              , estateFlatsThreeBedroom : 15
                              , estateSurfaceArea: SurfaceArea 12011.50
                              }

systemStateEx = SystemState { scale: { resolution: OneDay
                                     , scale: PersonScale
                                     , time: Month
                                     }
                            , state: State []
                            , systemParams: systemParamsEx
                            , processParams: initProcessParams
                            , current: NotImplemented
                          }

ds = unsafePartial $ canonicalDate (fromJust $ toEnum 2012) January (fromJust $ toEnum 1)
r1 = runReader (R.rainwaterHarvesting_tank ds) systemStateEx
