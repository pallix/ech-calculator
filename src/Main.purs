module Main where

import Prelude hiding (div)

import Calculator.Model (system)
import Calculator.Layout (layout)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Math (cos, sin, pi)
import Data.Int (toNumber)
import Data.Array (replicate, singleton)
import Data.Monoid (mempty)
import Data.Foldable (foldMap)

import DOM (DOM)
import Signal.Channel (CHANNEL)
import Graphics.Canvas (CANVAS)

import Control.Monad.Eff.Timer (TIMER)

import Graphics.Drawing (Point, rgb, rgba, translate, white)
import Graphics.Drawing.Font (font, sansSerif, bold)

import Text.Smolder.HTML (div, li, ul, table, td, tr, ul, li, p, h1, a, img, style)
import Text.Smolder.Markup (on, (#!), Markup, with, text, (!))
import Text.Smolder.HTML.Attributes (lang, charset, httpEquiv, content, name, rel, href, className, src)

import Signal.DOM (animationFrame)
import Signal.Time (since)

import Flare (UI, lift, numberSlider, liftSF, button, boolean_)

-- import Flare.Drawing (Drawing, Color, runFlareDrawing, fillColor,outlineColor, filled, closed, outlined, lineWidth, path)

import Flare.Smolder (runFlareHTML)

--
-- center :: Point
-- center = { x : 400.0, y: 300.0 }
--
-- delta :: Point
-- delta = { x : 200.0, y: 0.0 }
--
-- blue :: Color
-- blue = rgb 0 0 255
--
-- polygon :: Int -> Color -> Drawing
-- polygon sides col =
--   filled (fillColor col) $
--     closed $ poly sides
--
--   where poly n = do
--                     i <- 0..n
--                     let theta = pi / ( toNumber n / 2.0 ) * toNumber i
--                     pure { x: 50.0 * sin theta, y: 50.0 * cos theta }
--
-- hexagon :: Color -> Drawing
-- hexagon col = polygon 6 col
--
-- token :: String -> Color -> Drawing
-- token t col = hexagon col <> text ( font sansSerif 16 bold ) 0.0 0.0 (fillColor white) t
--
-- arrow :: Number -> Number -> Drawing
-- arrow v t = outlined (outlineColor (rgba 0 0 255 opacity) <> (lineWidth v )) (
--               path [ { x: arrowBegin.x , y: arrowBegin.y },
--                      { x: arrowEnd.x   , y: arrowEnd.y } ])
--            <> outlined (outlineColor (rgba 0 0 255 opacity) <> (lineWidth v )) (
--                 path [ { x: arrowEnd.x - arrowSize  , y: arrowEnd.y - arrowSize },
--                        { x: arrowEnd.x   , y: arrowEnd.y } ])
--            <> outlined (outlineColor (rgba 0 0 255 opacity) <> (lineWidth v )) (
--                 path [ { x: arrowEnd.x - arrowSize , y: arrowEnd.y + arrowSize },
--                        { x: arrowEnd.x , y: arrowEnd.y } ])
--               where
--                 opacity = ( cos ( t / 300.0 ) / 2.0 ) + 1.0
--                 arrowBegin = { x: (center.x - delta.x + ( delta.x / 4.0 ) ), y: ( center.y - delta.y + ( delta.y / 4.0 ) ) }
--                 arrowEnd = { x: (center.x + delta.x - ( delta.x / 4.0 ) ), y: ( center.y + delta.y - ( delta.y / 4.0 ) ) }
--                 arrowSize = 20.0
--
--
-- foodToBin :: Number -> Number -> Drawing
-- foodToBin v t = ( translateLeft ( token "Food" blue ) )
--              <> ( translateRight ( token "Bin" blue ) )
--              <> ( arrow v t )
--              <> ( text ( font sansSerif 16 bold ) center.x ( center.y - 10.0 )  (fillColor blue) (show v) )
--               where
--                 translateRight = translate ( center.x + delta.x ) ( center.y + delta.y )
--                 translateLeft = translate ( center.x - delta.x ) ( center.y - delta.y )
--
-- uidrawing :: forall e. UI (timer :: TIMER | e) Drawing
-- uidrawing = foodToBin <$> (numberSlider "value"  0.0 10.0 1.0  7.0)
--                <*> lift animationFrame
--

light :: forall e. Boolean -> Markup e
light on = div ! arg $ mempty
  where arg | on = className "on"
            | otherwise = mempty

hex :: forall e. Boolean -> String -> Markup e
hex on item = li ! className "hex" $ do
                token item
              where
                hoverClass true = className "hexIn"
                hoverClass false = className "hexIn hover"
                token "" = a ! hoverClass on $ mempty
                token _  = a ! hoverClass on $ do
                             img ! src ( image item )
                             h1 $ text item
                             p $ text "Some sample text on the hexagon"
                image "Food" = "https://farm5.staticflickr.com/4144/5053682635_b348b24698.jpg"
                image "Bin" = "https://farm5.staticflickr.com/4144/5053682635_b348b24698.jpg"
                image _ = ""

arrayBinToFood :: Array String
arrayBinToFood = ( replicate 10 "" )
              <> ( replicate  9 "" )
              <> ( replicate 10 "" )
              <> ( replicate  2 "" ) <> singleton "Food" <> ( replicate 3 "" ) <> singleton "Bin" <> ( replicate 2 "" )
              <> ( replicate 10 "" )
              <> ( replicate  9 "" )


hexes :: forall e. Boolean -> Markup e
hexes on = do
             ul ! className "hexGrid" $ do
               foldMap ( hex on ) arrayBinToFood

-- test :: forall a e. Markup (a -> Eff (console :: CONSOLE | e) Unit)
-- test = with div (className "on") $ do
--               h1 #! on "click" (\_ -> log "click") $ text "OMG HAI LOL"
--               p $ text "This is clearly the best HTML DSL ever invented.<script>alert(\"lol pwned\");</script>"

ui :: forall e e'. UI e (Markup e')
ui = ( pure $ style (text layout) )
   <> hexes <$> boolean_ false

-- <> light <$> liftSF (since 1000.0) (button "Switch on" unit unit)

-- ui = token <$> string_ "Yo"
--            <*> (color "Color" (hsl 333.0 0.6 0.5))


main :: Eff (dom :: DOM, channel :: CHANNEL, canvas :: CANVAS, timer :: TIMER, console :: CONSOLE) Unit
main = do
  runFlareHTML    "controls"  "output"  ui
  -- runFlareDrawing "controls1" "output1" uidrawing
