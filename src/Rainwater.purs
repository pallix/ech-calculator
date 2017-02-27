module Rainwater where

import Prelude
import Calculator.Layout (interface)
import Calculator.Model ( nexusSystem
                        , initProcessParams
                        , State(..)
                        , Matter(..)
                        , MatterProperty(..)
                        , Entry(..)
                        , SystemState(..)
                        , Scale(..)
                        , Time(..)
                        , SystemScale(..)
                        , Quantity(..)
                        , Ratio(..)
                        , Process(..)
                        , Transform(..)
                        , SystemParams(..)
                        , ProcessParams(..)
                        , Options(..)
                        , SurfaceArea(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Debug.Trace (spy)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Array (cons, snoc)
import Data.Foldable (foldMap)
import Data.Int (toNumber, fromNumber)
import Data.Tuple (Tuple(..))
import Data.Maybe (maybe)
import Data.Monoid (mempty)
import Data.Monoid.Additive (Additive(Additive))
import Data.Monoid.Multiplicative (Multiplicative(Multiplicative))
import Data.NonEmpty ((:|))
import Data.Traversable (traverse)
import Flare (UI, lift, fieldset, numberSlider, intSlider, liftSF, select, button, buttons, boolean, string, radioGroup, foldp, (<**>), runFlareWith)
import Flare.Smolder (runFlareHTML)
import Graphics.Canvas (CANVAS)
import Graphics.Drawing (Point, rgb, rgba, translate, white)
import Graphics.Drawing.Font (font, sansSerif, bold)
import Math (cos, sin, pi)
import Signal.Channel (CHANNEL)
import Signal.DOM (animationFrame)
import Signal.Time (since)
import Text.Smolder.Markup (on, (#!), Markup, with, text, (!))

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


optionsLabel Tank = "Tank"
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

nexusOptions = select "Options" (Tank :| [ EatingBinning
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
                                           }

systemParams = systemParamsWithConstants ( 0 )

initState = State [ Entry {process: Shopping, matter: Food, matterProperty: Shopped, quantity: Weight Food 585.0}
                    -- surface are of the estate = 12 000m² * 1000mm (1Meter) of water in Liters
                  , Entry {process: Raining, matter: Water, matterProperty: GreyWater, quantity: Volume Water $ 12000.0 * 1000.0}
                  , Entry {process: TapWaterSupplying, matter: Water, matterProperty: TapWater, quantity: Volume Water 100000000000000000000000000000000000.0}
                  ]

scaleToString PersonScale = "Person"
scaleToString HouseholdScale = "HouseHold"
scaleToString EstateScale = "Estate"

timescaleToString Year = "Year"
timescaleToString Month  = "Month"
timescaleToString Day  = "Day"

controllableParam numberHouseholdEating
                  numberCompactors
                  numberWormeries
                  gardenSurface
                  roofSurface
                  numberSharingHouseholds = initProcessParams { eatingParam = initProcessParams.eatingParam { numberHouseholdEating = numberHouseholdEating }
                                                     , binningParam = initProcessParams.binningParam { numberCompactors = numberCompactors }
                                                     , wormCompostingParam = initProcessParams.wormCompostingParam { numberWormeries = numberWormeries  }
                                                     , foodGardeningParam = initProcessParams.foodGardeningParam { surfaceArea = gardenSurface }
                                                     , rainwaterCollectingParam = initProcessParams.rainwaterCollectingParam { surfaceArea = roofSurface }
                                                     , foodSharingParam = initProcessParams.foodSharingParam { numberSharingHouseholds = numberSharingHouseholds }}

ratio ( Ratio _ { ratio } ) = ratio

systemState :: Options -> SystemScale -> SystemParams -> ProcessParams -> State -> SystemState
systemState current scale systemParams processParams state = SystemState { scale, systemParams, processParams, current, state }

mkScale s t = { scale : s, time: t}

areaToInt :: SurfaceArea -> Number
areaToInt ( SurfaceArea surfaceArea ) = surfaceArea

ui :: forall e e'. UI e (Markup e')
ui = interface <$> ( boolean "Info" false )
               <*> ( boolean "Grid" false )
               <*> ( spy <$> nexusSystem <$> ( systemState <$> nexusOptions
                                                          <*> ( mkScale <$> (select "Scale" ( EstateScale :| [ HouseholdScale, PersonScale ]) scaleToString)
                                                                        <*> (select "Time" (  Day :| [ Month, Year ]) timescaleToString) )
                                                          <*> pure systemParams
                                                          <*> ( fieldset "Eating Parameters" ( controllableParam <$> ( intSlider "numberHouseholdEating" 0 121 ( initProcessParams.eatingParam.numberHouseholdEating ) )
                                                                                                                 <*> ( intSlider "numberCompactors" 0 121 ( initProcessParams.binningParam.numberCompactors ) )
                                                                                                                 <*> ( intSlider "numberWormeries" 0 10 ( initProcessParams.wormCompostingParam.numberWormeries ) )
                                                                                                                 <*> ( SurfaceArea <$> ( numberSlider "gardenSurface" 0.0 100.0 1.0 ( areaToInt initProcessParams.foodGardeningParam.surfaceArea ) ) )
                                                                                                                 <*> ( SurfaceArea <$> ( numberSlider "roofSurface" 0.0 100.0 1.0 ( areaToInt initProcessParams.rainwaterCollectingParam.surfaceArea ) ) )
                                                                                                                 <*> ( intSlider "numberSharingHouseholds" 0 121 ( initProcessParams.foodSharingParam.numberSharingHouseholds ) ) ) )
                                                          <*> ( pure initState ) ) )


main :: Eff (dom :: DOM, channel :: CHANNEL, canvas :: CANVAS, timer :: TIMER, console :: CONSOLE) Unit
-- main = runFlareWith "select" inner nexusOptions
main = runFlareHTML "flare-controls" "output" ui