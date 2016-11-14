module Main where

import Prelude

import Calculator.Model (system, Token)
import Calculator.Layout (interface)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Math (cos, sin, pi)
import Data.Array (cons, snoc)
import Data.Int (toNumber)
import Data.Monoid (mempty)
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

import Flare (UI, lift, numberSlider, liftSF, button, buttons, boolean, string, foldp, (<**>))

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

type State = Array Token

perform :: Action -> State -> State
perform Food = flip snoc { title: "Food" }
perform Bin = flip snoc { title: "Bin" }
perform Compost = flip snoc { title: "Compost" }
perform Garden = flip snoc { title: "Garden" }
perform FoodGarden = flip snoc { title: "Food Garden" }
perform Reset     = const []

controls = foldp (maybe id perform) [ { title: "Food" } ] $
            buttons [Food, Bin, Compost, Garden, FoodGarden, Reset] label

actions = string "Add item:" "Bin" <**> button "Add" (flip const) cons

list = foldp id ["Food"] actions

ui :: forall e e'. UI e (Markup e')
ui = interface <$> ( boolean "Info" true )
               <*> ( boolean "Grid" false )
               <*> controls

-- <> light <$> liftSF (since 1000.0) (button "Switch on" unit unit)

-- ui = token <$> string_ "Yo"
--            <*> (color "Color" (hsl 333.0 0.6 0.5))


main :: Eff (dom :: DOM, channel :: CHANNEL, canvas :: CANVAS, timer :: TIMER, console :: CONSOLE) Unit
main = do
  runFlareHTML    "controls"  "output"  ui
  -- runFlareDrawing "controls1" "output1" uidrawing
