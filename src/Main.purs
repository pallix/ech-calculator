module Main where

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
                        , Quantity(..)
                        , Ratio(..)
                        , Process(..)
                        , Transform(..)
                        , SystemParams(..)
                        , ProcessParams(..)
                        , Options(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Debug.Trace (spy)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Array (cons, snoc)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Data.Maybe (maybe)
import Data.Monoid (mempty)
import Data.Monoid.Additive (Additive(Additive))
import Data.Monoid.Multiplicative (Multiplicative(Multiplicative))
import Data.NonEmpty ((:|))
import Data.Traversable (traverse)
import Flare (UI, lift, fieldset, numberSlider, liftSF, select, button, buttons, boolean, string, radioGroup, foldp, (<**>), runFlareWith)
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


optionsLabel EatingOnly = "Food"
optionsLabel EatingBinning = "Food & Waste"
optionsLabel CompostingOnly = "Composting"
optionsLabel CompostingGarden = "Composting & Garden"
optionsLabel CompostingFoodGarden = "Composting & Food Garden"
optionsLabel WateringGarden = "Watering Garden"
optionsLabel RainwaterWateringGarden = "Rainwater Collection & Garden"
optionsLabel NotImplemented = "Not Implemented Yet"

nexusOptions = select "Options" (EatingOnly :| [ EatingBinning, CompostingOnly, CompostingGarden, CompostingFoodGarden, WateringGarden, RainwaterWateringGarden ] ) optionsLabel

systemParamsWithConstants = SystemParams <$> { houseHoldSize: _
                                           , estatePopulation : 200
                                           , estateFlatsOneBedroom : 70
                                           , estateFlatsTwoBedroom : 23
                                           , estateFlatsThreeBedroom : 15
                                           }

systemParams = systemParamsWithConstants ( 0 )

initState = State [ Entry {process: Shopping, matter: Food, matterProperty: Shopped, quantity: Weight Food 120.0}
                    , Entry {process: Shopping, matter: Food, matterProperty: Shopped, quantity: Weight Food (-20.0)}
                    , Entry {process: Eating, matter: Waste, matterProperty: NonEdible, quantity: Weight Waste 10.0} ]

-- eatingParam =  { title: "Eating"
--                , eatedFoodRatio: Ratio Food { ratio: 0.81 } -- 1 - allFoodWasteRatio
--                , allFoodWasteProcess: Transform Food Waste { ratio: 0.19 } -- ECH_LCA_Tool:Material Flow Summary!T7 + ECH_LCA_Tool:Material Flow Summary!U7
--                , edibleWasteRatio: Ratio Waste { ratio: 0.114 } -- ECH_LCA_Tool:Material Flow Summary!T7
--                , nonedibleFoodWasteRatio: Ratio Waste { ratio: 0.076 } -- ECH_LCA_Tool:Material Flow Summary!U7
--                }
--
-- binningParam = { title: "Binning"
--                , inputRatio: Ratio Waste { ratio: 1.0 }
--                , allFoodWasteProcess: Transform Food Waste { ratio: 0.19 } -- ECH_LCA_Tool:Material Flow Summary!T7 + ECH_LCA_Tool:Material Flow Summary!U7
--                }

scaleToString PersonScale = "Person"
scaleToString HouseholdScale  = "HouseHold"
scaleToString EstateScale  = "Estate"

controllableParam eatedFoodRatio = initProcessParams { eatingParam = initProcessParams.eatingParam { eatedFoodRatio = Ratio Food { ratio: eatedFoodRatio } } }

systemState :: Options -> Scale -> SystemParams -> ProcessParams ->  State -> SystemState
systemState current scale systemParams processParams  state = SystemState { scale, systemParams, processParams, current, state }

-- ui :: forall e e'. UI e (Markup e')
--
ui = interface <$> ( boolean "Info" true )
               <*> ( boolean "Grid" false )
               <*> ( spy <$> ( nexusSystem <$> ( systemState <$> nexusOptions
                                                              <*> (select "Scale" (PersonScale :| [HouseholdScale, EstateScale]) scaleToString)
                                                              <*> pure systemParams
                                                              <*> fieldset "Eating Parameters" ( controllableParam <$> ( numberSlider "eatedFoodRatio" 0.0 1.0 0.01 0.81 ) )
                                                              <*> ( pure initState ) ) ) )

--
-- ui opt = interface <$> ( boolean "Info" true )
--                        <*> ( boolean "Grid" false )
--                        <*> pure opt
--                        <*> ( nexusSystem  <$> (select "Scale" (PersonScale :| [HouseholdScale, EstateScale]) scaleToString)
--                                           <*> pure systemParam
--                                           <*> fieldset ( ( optionsLabel opt ) <> "Parameters" ) ( controllableParam <$> ( numberSlider "eatedFoodRatio" 0.0 1.0 0.01 0.81 ) )
--                                           <*> pure eatingInitState
--                                           <*> pure opt )
--
-- ui EatingBinning = interface <$> ( boolean "Info" true )
--                    <*> ( boolean "Grid" false )
--                    <*> pure EatingBinning
--                    <*> ( nexusSystem  <$> (select "Scale" (PersonScale :| [HouseholdScale, EstateScale]) scaleToString)
--                          <*> pure systemParam
--                          <*> fieldset "Eating Binning Parameters" ( controllableParam <$> ( numberSlider "eatedFoodRatio" 0.0 1.0 0.01 0.81 ) )
--                          <*> pure eatingInitState
--                          <*> pure EatingBinning )
--
-- ui _ = interface <$> ( boolean "Info" true )
--                  <*> ( boolean "Grid" false )
--                  <*> ( nexusSystem <$> pure PersonScale
--                                    <*> pure systemParam
--                                    <*> pure ( controllableParam 0.0 )
--                                    <*> pure eatingInitState
--                                    <*> pure NotImplemented )

-- inner = runFlareHTML "controls" "output" <<< ui

-- <> light <$> liftSF (since 1000.0) (button "Switch on" unit unit)

-- ui = token <$> string_ "Yo"
--            <*> (color "Color" (hsl 333.0 0.6 0.5))

-- Below is an example of what I think the applicative interface results in
-- where test is instantiated twice with both instance being completely independent.
--
-- ui :: forall e e'. UI e (Markup e')
-- ui = ( interface <$> ( boolean "Info" true )
--                <*> ( test )
--                <*> ( eatingBinningUI )
--                <*> ( optionsTokens <$> options ) )
--   <> ( text <$>
--         ( show <$> test ) )
--   where
--     test = boolean "Test" false
--

main :: Eff (dom :: DOM, channel :: CHANNEL, canvas :: CANVAS, timer :: TIMER, console :: CONSOLE) Unit
-- main = runFlareWith "select" inner nexusOptions
main = runFlareHTML "controls" "output" ui

  -- runFlareWith "controls" "output" ui2
  -- runFlareDrawing "controls1" "output1" uidrawing
