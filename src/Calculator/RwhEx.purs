module RwhEx where

import Data.Date.Component
import Control.Monad
import Calculator.Rwh as R
import Calculator.Nexus
import Calculator.Model (Options(..), Process(..), Scale(..), State(..), SurfaceArea(..), SystemParams(..), SystemState(..), Time(..), initProcessParams)
import Control.Monad.Reader (runReader)
import Data.Date (Date, Month(..), canonicalDate, day, month, year)
import Data.Enum (succ, toEnum)
import Data.Maybe (fromJust, maybe, Maybe(..))
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (unfoldr)
import Partial.Unsafe (unsafePartial)
import Prelude (bottom, id, ($), (<=))
import Time (TimeResolution(..), TimeWindow(..))


systemParamsEx = SystemParams { houseHoldSize: 199
                              , estatePopulation : 200
                              , estateAveragePersonPerHousehold : 2.4
                              , estateFlatsOneBedroom : 70
                              , estateFlatsTwoBedroom : 23
                              , estateFlatsThreeBedroom : 15
                              , estateSurfaceArea: SurfaceArea 12011.50
                              }

dStart = unsafePartial $ canonicalDate (fromJust $ toEnum 2012) January (fromJust $ toEnum 1)
dStop = unsafePartial $ canonicalDate (fromJust $ toEnum 2012) January (fromJust $ toEnum 2)

systemStateEx = SystemState { scale: { resolution: OneDay
                                     , scale: PersonScale
                                     , time: Month -- old scale system, TODO deprecate
                                     , window: TimeWindow { start: dStart
                                                          , end: dStop
                                     }
                                     }
                            , state: State []
                            , systemParams: systemParamsEx
                            , processParams: initProcessParams
                            , current: RainwaterHarvestingDemand
                          }

r1 = runReader (R.rainwaterHarvesting_tank dStart) systemStateEx
r3 = runReader (R.raining dStop) systemStateEx
r2 = scanNexus systemStateEx