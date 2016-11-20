module Main where

import Prelude

import Calculator.Model (Token, Flow(Flow), nexusSystem, flowParams, State(..), Scale(..), Quantity(..), Ratio(..), Food(..), Waste(..), Stock(..), SystemParam(..), Options(..))
import Calculator.Layout (interface)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Math (cos, sin, pi)
import Data.Array (cons, snoc)
import Data.Int (toNumber)
import Data.NonEmpty ((:|))
import Data.Monoid (mempty)
import Data.Monoid.Additive (Additive(Additive))
import Data.Monoid.Multiplicative (Multiplicative(Multiplicative))
import Data.Maybe (maybe)
import Data.Foldable (foldMap)
import Data.Traversable (traverse)

import DOM (DOM)
import Signal.Channel (CHANNEL)
import Graphics.Canvas (CANVAS)

import Control.Monad.Eff.Timer (TIMER)

import Graphics.Drawing (Point, rgb, rgba, translate, white)
import Graphics.Drawing.Font (font, sansSerif, bold)

import Text.Smolder.Markup (on, (#!), Markup, with, text, (!))

import Signal.DOM (animationFrame)
import Signal.Time (since)

import Flare (UI, lift, fieldset, numberSlider, liftSF, select, button, buttons, boolean, string, radioGroup, foldp, (<**>))

-- import Flare.Drawing (Drawing, Color, runFlareDrawing, fillColor,outlineColor, filled, closed, outlined, lineWidth, path)

import Flare.Smolder (runFlareHTML)

data Action = Food
            | Bin
            | Compost
            | Garden
            | FoodGarden
            | Reset

label :: Action -> String
label Food = "Food"
label Bin = "Bin"
label Compost = "Compost"
label Garden = "Garden"
label FoodGarden = "FoodGarden"
label Reset = "Reset"

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


optionsLabel Eating = "Food"
optionsLabel EatingBinning = "Food & Waste"
optionsLabel Composting = "Composting"
optionsLabel CompostingGarden = "Composting & Garden"
optionsLabel CompostingFoodGarden = "Composting & Food Garden"
optionsLabel WateringGarden = "Watering Garden"
optionsLabel RainwaterWateringGarden = "Rainwater Collection & Garden"

options = select "Options" (Eating :| [ EatingBinning, Composting, CompostingGarden, CompostingFoodGarden, WateringGarden, RainwaterWateringGarden ] ) optionsLabel

systemParamWithConstants = SystemParam <$> { houseHoldSize: _
                                           , estatePopulation : 200
                                           , estateFlatsOneBedroom : 70
                                           , estateFlatsTwoBedroom : 23
                                           , estateFlatsThreeBedroom : 15
                                           }

systemParam = systemParamWithConstants ( 0 )

eatingInitState = State { shoppedFood: Stock ( Weight ShoppedFood 585.0 ) ( Weight ShoppedFood 0.0 )
                        , binnedFoodWaste: Stock ( Weight FoodWaste 0.0 ) ( Weight FoodWaste 0.0 )
                        , managedWaste: Stock ( Weight ManagedWaste 0.0 ) ( Weight ManagedWaste 0.0 )
                        , sharedFood: Stock ( Weight SharedFood 0.0 ) ( Weight SharedFood 0.0 )
                        }


eatingParam =  { title: "Eating"
               , eatedFoodRatio: Ratio AnyFood { ratio: 0.81 } -- 1 - allFoodWasteRatio
               , allFoodWasteRatio: Ratio AnyFood { ratio: 0.19 } -- ECH_LCA_Tool:Material Flow Summary!T7 + ECH_LCA_Tool:Material Flow Summary!U7
               , edibleWasteRatio: Ratio AnyFood { ratio: 0.114 } -- ECH_LCA_Tool:Material Flow Summary!T7
               , nonedibleFoodWasteRatio: Ratio AnyFood { ratio: 0.076 } -- ECH_LCA_Tool:Material Flow Summary!U7
               }

eatingBinningInput = Flow { input : 0
                         , output: 0
                         , stock: 0
                         }

scaleToString PersonScale = "Person"
scaleToString HouseholdScale  = "HouseHold"
scaleToString EstateScale  = "Estate"

controllableParam eatedFoodRatio = flowParams { eatingParam = eatingParam { eatedFoodRatio = Ratio AnyFood { ratio: eatedFoodRatio } } }

nexusSystemUI = nexusSystem
              <$> (select "Scale" (PersonScale :| [HouseholdScale, EstateScale]) scaleToString)
              <*> pure systemParam
              <*> fieldset "Eating Parameters" ( controllableParam <$> ( numberSlider "eatedFoodRatio" 0.0 1.0 0.01 0.81 ) )
              <*> options
              <*> pure eatingInitState

ui :: forall e e'. UI e (Markup e')
ui = interface <$> ( boolean "Info" true )
               <*> ( boolean "Grid" false )
               <*> ( nexusSystemUI )

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
main = do
  runFlareHTML    "controls"  "output"  ui
  -- runFlareWith "controls" "output" ui2
  -- runFlareDrawing "controls1" "output1" uidrawing
