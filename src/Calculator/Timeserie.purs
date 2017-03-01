module Calculator.Timeserie where

import Data.Maybe (Maybe)
import Time (TimeInterval)


type Timeserie a = TimeInterval -> Maybe a

--  :: forall a. Array Interval -> Array a
-- buildValues = u
-- map () interval
