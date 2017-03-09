module Rainwater where

import Prelude
import Data.DateTime as DT
import Calculator.Layout (interface)
import Calculator.Model (initProcessParams, State(..), Matter(..), MatterProperty(..), Entry(..), SystemState(..), Scale(..), Time(..), SystemScale(..), Quantity(..), Ratio(..), Process(..), Transform(..), SystemParams(..), ProcessParams(..), Options(..), SurfaceArea(..))
import Calculator.Nexus (nexusSystem, scanNexus)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Array (cons, snoc, last, head)
import Data.Date (Date, Month(..), canonicalDate, day, month, year)
import Data.Enum (succ, toEnum)
import Data.Foldable (foldMap)
import Data.Int (toNumber, fromNumber)
import Data.Map (empty)
import Data.Maybe (fromJust, maybe, Maybe(..))
import Data.Maybe (maybe)
import Data.Monoid (mempty)
import Data.Monoid.Additive (Additive(Additive))
import Data.Monoid.Multiplicative (Multiplicative(Multiplicative))
import Data.NonEmpty ((:|))
import Data.Time.Duration (Days(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Flare (UI, lift, fieldset, numberSlider, intSlider, liftSF, select, button, buttons, boolean, string, radioGroup, foldp, (<**>), runFlareWith)
import Flare.Smolder (runFlareHTML)
import Graphics.Canvas (CANVAS)
import Graphics.Drawing (Point, rgb, rgba, translate, white)
import Graphics.Drawing.Font (font, sansSerif, bold)
import Math (cos, sin, pi)
import Partial.Unsafe (unsafePartial)
import Signal.Channel (CHANNEL)
import Signal.DOM (animationFrame)
import Signal.Time (since)
import Text.Smolder.Markup (on, (#!), Markup, with, text, (!))
import Time (TimeInterval(..), TimePeriod(..), TimeWindow(..), dates, defaultTi)

-- data Action = Food
--             | Bin
--             | Compost
--             | Garden
--             | FoodGarden
--             | Reset
--
-- label :: Action -> String
-- label Food = "Food"
-- label Bin = "Bin"
-- label Compost = "Compost"
-- label Garden = "Garden"
-- label FoodGarden = "FoodGarden"
-- label Reset = "Reset"

-- type State = Array Token

-- perform :: Action -> State -> State
-- perform Food = flip snoc { title: "Food" }
-- perform Bin = flip snoc { title: "Bin" }
-- perform Compost = flip snoc { title: "Compost" }
-- perform Garden = flip snoc { title: "Garden" }
-- perform FoodGarden = flip snoc { title: "Food Garden" }
-- perform Reset     = const []

-- TODO: Reuse this traverseable approach to create `optionals` and `booleans` functions
-- controls = foldp (maybe id perform) [ { title: "Food" } ] $
--             buttons [Food, Bin, Compost, Garden, FoodGarden, Reset] label
-- actions = string "Add item:" "Bin" <**> button "Add" (flip const) cons
-- list = foldp id ["Food"] actions


optionsLabel RainwaterHarvestingTank = "Rainwater Tank"
optionsLabel RainwaterHarvestingDemand = "Rainwater Usage"
optionsLabel RainwaterHarvestingCollection = "Rainwater Collection"
optionsLabel EatingBinning = "Food & Waste"
optionsLabel EatingBinningWormComposting = "Wormery"
-- optionsLabel EatingBinningWormCompostingGarden = "Wormery & Garden"
optionsLabel EatingBinningWormCompostingFoodGardening = "Wormery & Food Garden"
-- optionsLabel EatingBinningWormCompostingGardenWatering = "Garden Watering "
optionsLabel EatingBinningWormCompostingFoodGardenWatering = "Food Garden Watering "
-- optionsLabel EatingBinningWormCompostingGardenRainwater = "Rainwater Collection & Garden"
optionsLabel EatingBinningWormCompostingFoodGardenRainwater = "Rainwater Collection"

optionsLabel EatingBinningFoodSharing = "Food Sharing"
optionsLabel EatingBinningWormCompostingFoodSharing = "Food Sharing"
optionsLabel _ = "Not Implemented Yet"

nexusOptions = select "Options" (RainwaterHarvestingTank :| [ EatingBinning
                                              --  , EatingBinningWormComposting
                                               , EatingBinningWormCompostingFoodGardening
                                              --  , EatingBinningWormCompostingFoodGardenWatering
                                               , EatingBinningWormCompostingFoodGardenRainwater
                                              --  , EatingBinningFoodSharing
                                               , EatingBinningWormCompostingFoodSharing ] ) optionsLabel

systemParamsWithConstants = SystemParams <$> { houseHoldSize: _
                                           , estatePopulation : 200
                                           , estateAveragePersonPerHousehold : 2.4
                                           , estateFlatsOneBedroom : 70
                                           , estateFlatsTwoBedroom : 23
                                           , estateFlatsThreeBedroom : 15
                                           , estateSurfaceArea: SurfaceArea 12011.50
                                           }

dStart = unsafePartial $ canonicalDate (fromJust $ toEnum 2012) January (fromJust $ toEnum 1)
dStop = unsafePartial $ canonicalDate (fromJust $ toEnum 2013) January (fromJust $ toEnum 1)

systemParams = systemParamsWithConstants ( 0 )

initState = State [ Entry {process: Shopping, matter: Food, matterProperty: Shopped, quantity: Weight Food 585.0, interval: defaultTi}
                    -- surface are of the estate = 12 000m² * 1000mm (1Meter) of water in Liters
                  , Entry {process: Raining, matter: Water, matterProperty: GreyWater, quantity: Volume Water $ 12000.0 * 1000.0, interval: defaultTi}
                  , Entry {process: TapWaterSupplying, matter: Water, matterProperty: TapWater, quantity: Volume Water 100000000000000000000000000000000000.0, interval: defaultTi}
                  ]

scaleToString PersonScale = "Person"
scaleToString HouseholdScale = "HouseHold"
scaleToString EstateScale = "Estate"

timescaleToString Year = "Year"
timescaleToString Month  = "Month"
timescaleToString Day  = "Day"

timePeriodToString OneDay = "Day"
-- timePeriodToString OneWeek = "Week"
timePeriodToString OneMonth = "Month"


windowChoiceToString Default = "Default"

chooseWindow Default = TimeWindow {start: dStart, end: dStop}

data WindowChoice = Default

controllableParam numberHouseholdEating
                  numberCompactors
                  numberWormeries
                  gardenSurface
                  numberOfBlocks
                  numberSharingHouseholds = initProcessParams { eatingParam = initProcessParams.eatingParam { numberHouseholdEating = numberHouseholdEating }
                                                     , binningParam = initProcessParams.binningParam { numberCompactors = numberCompactors }
                                                     , wormCompostingParam = initProcessParams.wormCompostingParam { numberWormeries = numberWormeries  }
                                                     , foodGardeningParam = initProcessParams.foodGardeningParam { surfaceArea = gardenSurface }
                                                     , rainwaterCollectingParam = initProcessParams.rainwaterCollectingParam { numberOfBlocks = numberOfBlocks }
                                                     , foodSharingParam = initProcessParams.foodSharingParam { numberSharingHouseholds = numberSharingHouseholds }}

ratio ( Ratio _ { ratio } ) = ratio

-- systemStateEx = SystemState { scale: { period: OneDay
--                                      , scale: PersonScale
--                                      , time: Month -- old scale system, TODO deprecate
--                                      , window: TimeWindow { start: dStart
--                                                           , end: dStop
--                                                           }
--                                      }
--                             , state: State []
--                             , systemParams: systemParamsEx
--                             , processParams: initProcessParams
--                             , current: RainwaterHarvestingCollection
--                             , timeseries: empty
--                           }

systemState :: Options -> SystemScale -> SystemParams -> ProcessParams -> State -> TimeInterval -> SystemState
systemState current scale systemParams processParams state interval = SystemState { scale
                                                                         , systemParams
                                                                         , processParams
                                                                         , current
                                                                         , state
                                                                         , timeseries: empty
                                                                         , interval
                                                                         }

mkScale s t p wc = { scale : s, time: t, period: p, window: chooseWindow wc}

areaToInt :: SurfaceArea -> Number
areaToInt ( SurfaceArea surfaceArea ) = surfaceArea

initInterval :: TimeInterval
initInterval = TimeInterval { date : dStart
                            , period : OneMonth
                            }

unsafeScanNexus = unsafePartial $ fromJust <<< last <<< scanNexus

ui :: forall e e'. UI e (Markup e')
ui = interface <$> ( boolean "Info" false )
               <*> ( boolean "Grid" false )
               <*> ( spy <$> unsafeScanNexus <$> ( systemState <$> nexusOptions
                                                          <*> ( mkScale <$> (select "Scale" ( EstateScale :| [ HouseholdScale, PersonScale ]) scaleToString)
                                                                        <*> (select "Time" (  Day :| [ ]) timescaleToString)
                                                                        <*> (select "Period" (  OneDay :| [ OneMonth ]) timePeriodToString)
                                                                        <*> (select "Window" (  Default :| [ ]) windowChoiceToString) )
                                                          <*> pure systemParams
                                                          <*> ( fieldset "Eating Parameters" ( controllableParam <$> ( intSlider "numberHouseholdEating" 0 121 ( initProcessParams.eatingParam.numberHouseholdEating ) )
                                                                                                                 <*> ( intSlider "numberCompactors" 0 121 ( initProcessParams.binningParam.numberCompactors ) )
                                                                                                                 <*> ( intSlider "numberWormeries" 0 10 ( initProcessParams.wormCompostingParam.numberWormeries ) )
                                                                                                                 <*> ( SurfaceArea <$> ( numberSlider "gardenSurface" 0.0 100.0 1.0 ( areaToInt initProcessParams.foodGardeningParam.surfaceArea ) ) )
                                                                                                                 <*> ( intSlider "numberOfBlocks" 0 10 ( initProcessParams.rainwaterCollectingParam.numberOfBlocks ) )
                                                                                                                 <*> ( intSlider "numberSharingHouseholds" 0 121 ( initProcessParams.foodSharingParam.numberSharingHouseholds ) ) ) )
                                                          <*> ( pure initState )
                                                          <*> ( pure initInterval) ) )


main :: Eff (dom :: DOM, channel :: CHANNEL, canvas :: CANVAS, timer :: TIMER, console :: CONSOLE) Unit
-- main = runFlareWith "select" inner nexusOptions
main = runFlareHTML "flare-controls" "output" ui
