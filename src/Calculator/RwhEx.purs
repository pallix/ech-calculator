module RwhEx where

import Data.Date.Component
import Control.Monad
import Calculator.Nexus
import Calculator.Rwh as R
import Calculator.Model (Options(..), Process(..), Scale(..), State(..), SurfaceArea(..), SystemParams(..), SystemState(..), Time(..), foldNotifications, initProcessParams)
import Calculator.Plot (plotData)
import Control.Monad.Reader (runReader)
import Data.Array (last, head)
import Data.Date (Date, Month(..), canonicalDate, day, month, year)
import Data.Enum (succ, toEnum)
import Data.Map (empty)
import Data.Maybe (fromJust, maybe, Maybe(..))
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (unfoldr)
import Partial.Unsafe (unsafePartial)
import Prelude (bottom, id, show, ($), (<=))
import Time (TimeInterval(..), TimePeriod(..), TimeWindow(..), dates)


systemParamsEx = SystemParams { houseHoldSize: 199
                              , estatePopulation : 200
                              , estateAveragePersonPerHousehold : 2.4
                              , estateFlatsOneBedroom : 70
                              , estateFlatsTwoBedroom : 23
                              , estateFlatsThreeBedroom : 15
                              , estateSurfaceArea: SurfaceArea 12011.50
                              }

dStart = unsafePartial $ canonicalDate (fromJust $ toEnum 2012) January (fromJust $ toEnum 1)
dStop = unsafePartial $ canonicalDate (fromJust $ toEnum 2012) December (fromJust $ toEnum 3)

systemStateEx = SystemState { scale: { period: OneMonth
                                     , scale: PersonScale
                                     , time: Month -- old scale system, TODO deprecate
                                     , window: TimeWindow { start: dStart
                                                          , end: dStop
                                     }
                                     }
                            , state: State []
                            , systemParams: systemParamsEx
                            , processParams: initProcessParams
                            , current: RainwaterHarvestingCollection
                            , timeseries: empty
                          }

r1 = runReader (R.harvestingRainwaterWithOpenedTank (TimeInterval { date: dStart, period: OneDay})) systemStateEx

-- r3 = runReader (R.raining dStop) systemStateEx

nexus = scanNexus systemStateEx
r2 = unsafePartial $ fromJust $ last $ scanNexus systemStateEx
r2n (SystemState st) = foldNotifications StoringRainwater st.state
