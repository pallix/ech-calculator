module Calculator.Layout ( layout ) where


import Prelude
import Data.Maybe (Maybe(Nothing, Just))
import CSS (Rendered, color, display, renderedSheet, block, render, body, blue, (?), fromString, mediaQuery)


example4 :: Rendered
example4 = render do
  body ? do
    color blue
  fromString "#world" ? do
    display block

toCss :: Rendered -> String
toCss r = case renderedSheet r of
              Nothing -> ""
              Just a -> a

layout :: String
layout = toCss example4
