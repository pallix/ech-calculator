module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Math (cos, sin, pi)
import Data.Int (toNumber)
import Data.Array ((..))

import DOM (DOM)
import Signal.Channel (CHANNEL)
import Graphics.Canvas (CANVAS)

import Control.Monad.Eff.Timer (TIMER)

import Flare (UI, lift, numberSlider)

import Graphics.Drawing (Point, rgb, rgba, translate, white)
import Graphics.Drawing.Font (font, sansSerif, bold)

import Signal.DOM (animationFrame)

import Flare.Drawing (Drawing, Color, runFlareDrawing, fillColor,outlineColor, filled, text, closed, outlined, lineWidth, path)


center :: Point
center = { x : 400.0, y: 300.0 }

delta :: Point
delta = { x : 200.0, y: 0.0 }

blue :: Color
blue = rgb 0 0 255

polygon :: Int -> Color -> Drawing
polygon sides col =
  filled (fillColor col) $
    closed $ poly sides

  where poly n = do
                    i <- 0..n
                    let theta = pi / ( toNumber n / 2.0 ) * toNumber i
                    pure { x: 50.0 * sin theta, y: 50.0 * cos theta }

hexagon :: Color -> Drawing
hexagon col = polygon 6 col

token :: String -> Color -> Drawing
token t col = hexagon col <> text ( font sansSerif 16 bold ) 0.0 0.0 (fillColor white) t

arrow :: Number -> Number -> Drawing
arrow v t = outlined (outlineColor (rgba 0 0 255 opacity) <> (lineWidth v )) (
              path [ { x: arrowBegin.x , y: arrowBegin.y },
                     { x: arrowEnd.x   , y: arrowEnd.y } ])
           <> outlined (outlineColor (rgba 0 0 255 opacity) <> (lineWidth v )) (
                path [ { x: arrowEnd.x - arrowSize  , y: arrowEnd.y - arrowSize },
                       { x: arrowEnd.x   , y: arrowEnd.y } ])
           <> outlined (outlineColor (rgba 0 0 255 opacity) <> (lineWidth v )) (
                path [ { x: arrowEnd.x - arrowSize , y: arrowEnd.y + arrowSize },
                       { x: arrowEnd.x , y: arrowEnd.y } ])
              where
                opacity = ( cos ( t / 300.0 ) / 2.0 ) + 1.0
                arrowBegin = { x: (center.x - delta.x + ( delta.x / 4.0 ) ), y: ( center.y - delta.y + ( delta.y / 4.0 ) ) }
                arrowEnd = { x: (center.x + delta.x - ( delta.x / 4.0 ) ), y: ( center.y + delta.y - ( delta.y / 4.0 ) ) }
                arrowSize = 20.0


foodToBin :: Number -> Number -> Drawing
foodToBin v t = ( translateLeft ( token "Food" blue ) )
             <> ( translateRight ( token "Bin" blue ) )
             <> ( arrow v t )
             <> ( text ( font sansSerif 16 bold ) center.x ( center.y - 10.0 )  (fillColor blue) (show v) )
              where
                translateRight = translate ( center.x + delta.x ) ( center.y + delta.y )
                translateLeft = translate ( center.x - delta.x ) ( center.y - delta.y )

ui :: forall e. UI (timer :: TIMER | e) Drawing
ui = foodToBin <$> (numberSlider "value"  0.0 10.0 1.0  7.0)
               <*> lift animationFrame

-- ui = token <$> string_ "Yo"
--            <*> (color "Color" (hsl 333.0 0.6 0.5))


main :: Eff (dom :: DOM, channel :: CHANNEL, canvas :: CANVAS, timer :: TIMER) Unit
main = do
  runFlareDrawing "controls" "output" ui
