module Calculator.Layout (interface) where

import Calculator.Model (Food, Waste, Token, Flow2,
  Options(Eating, RainwaterWateringGarden, WateringGarden, CompostingFoodGarden, CompostingGarden, Composting, EatingBinning, NotImplemented),
  Quantity(IncompatibleQuantity, Volume, Weight),
  State(State), Stock(Stock), Food(..), Waste(..))

import Math (trunc)
import CSS (darkgrey, Rendered, color, display, renderedSheet, block, render, body, blue, (?), fromString, mediaQuery)
import DOM.Node.Types (documentTypeToNode)
import Data.Array (replicate, singleton)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid (mempty)
import Data.Generic
import Text.Smolder.HTML (div, li, ul, table, td, tr, ul, li, p, h2, a, img, style)
import Text.Smolder.HTML.Attributes (lang, charset, httpEquiv, content, name, rel, href, className, src)
import Text.Smolder.Markup (on, (#!), Markup, with, text, (!))
import Prelude hiding (div)


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

css :: forall e. Markup e
css = style (text layout)

-- light :: forall e. Boolean -> Markup e
-- light on = div ! arg $ mempty
--   where arg | on = className "on"
--             | otherwise = mempty

hex :: forall e. Boolean -> Boolean -> Token -> Markup e
hex hover grid item = li ! className "hex" $ do
                        tokenToHex item
                      where
                        hoverClass false false = className "hexIn"
                        hoverClass true false = className "hexIn hover"
                        hoverClass _ true = className "hexIn grid"
                        tokenToHex { title: "" } = a ! hoverClass hover grid $ mempty
                        tokenToHex { title } = a ! hoverClass hover grid $ do
                                     img ! src ( image item.title )
                                     h2 $ text title
                                     p $ text "Details"
                        tokenToHex _  = a ! hoverClass hover grid $ do
                                     img ! src ( image item.title )
                        image "Eating" = "https://farm5.staticflickr.com/4144/5053682635_b348b24698.jpg"
                        image "Binning" = "https://farm5.staticflickr.com/4144/5053682635_b348b24698.jpg"
                        image "Compost" = "https://farm5.staticflickr.com/4144/5053682635_b348b24698.jpg"
                        image "Garden" = "https://farm5.staticflickr.com/4144/5053682635_b348b24698.jpg"
                        image "Food Garden" = "https://farm5.staticflickr.com/4144/5053682635_b348b24698.jpg"
                        image "Shopped Food" = "https://farm5.staticflickr.com/4144/5053682635_b348b24698.jpg"
                        image "_" = "https://dummyimage.com/200x200&text=+"
                        image _ = ""

flow :: forall e. Flow2 -> Markup e
flow item = tokenToHex item
            where
              tokenToHex { title: "_", quantity: n } = li ! className "hex" $ do
                      a ! className "hexIn hover" $ do
                           h2 $ text $ show n
                           p $ text ""
                           div ! className "arrow-css right" $ mempty
              tokenToHex { title: "/", quantity: n } = li ! className "hex rotate-1" $ do
                      a ! className "hexIn hover" $ do
                           h2 $ text $ show n
                           p $ text ""
                           div ! className "arrow-css right" $ mempty
                          --  img ! src "https://dummyimage.com/200x200&text=+"
              tokenToHex _  = li ! className "hex" $ do
                      a ! className "hexIn hover" $ mempty -- $ do
                          --  img ! src "https://dummyimage.com/200x200&text=+"

arrayHex :: Array Token -> Array Token
arrayHex [ a ]  =  ( replicate 10 { title: "" } )
                <> ( replicate  9 { title: "" } )
                <> ( replicate 10 { title: "" } )
                <> ( replicate  4 { title: "" } ) <> singleton a <> ( replicate 4 { title: "" } )
                <> ( replicate 10 { title: "" } )
                <> ( replicate  9 { title: "" } )


arrayHex [ a, b ]  = ( replicate 10 { title: "" } )
                  <> ( replicate  9 { title: "" } )
                  <> ( replicate 10 { title: "" } )
                  <> ( replicate  3 { title: "" } ) <> singleton a <> ( replicate 1 { title: "" } ) <> singleton b <> ( replicate 3 { title: "" } )
                  <> ( replicate 10 { title: "" } )
                  <> ( replicate  9 { title: "" } )

arrayHex [ a, b, c ]  = ( replicate 10 { title: "" } )
                     <> ( replicate  4 { title: "" } ) <> singleton c <> ( replicate  4 { title: "" } )
                     <> ( replicate 10 { title: "" } )
                     <> ( replicate  3 { title: "" } ) <> singleton a <> ( replicate 1 { title: "" } ) <> singleton b <> ( replicate 3 { title: "" } )
                     <> ( replicate 10 { title: "" } )
                     <> ( replicate  9 { title: "" } )


arrayHex [ a, b, c, d ]  = ( replicate 10 { title: "" } )
                        <> ( replicate  4 { title: "" } ) <> singleton c <> ( replicate  4 { title: "" } )
                        <> ( replicate 10 { title: "" } )
                        <> ( replicate  3 { title: "" } ) <> singleton a <> ( replicate 1 { title: "" } ) <> singleton b <> ( replicate 3 { title: "" } )
                        <> ( replicate 10 { title: "" } )
                        <> ( replicate  4 { title: "" } ) <> singleton d <> ( replicate  4 { title: "" } )

arrayHex _  = ( replicate 10 { title: "" } )
           <> ( replicate  9 { title: "" } )
           <> ( replicate 10 { title: "" } )
           <> ( replicate  9 { title: "" } )
           <> ( replicate 10 { title: "" } )
           <> ( replicate  9 { title: "" } )

hexes :: forall e. Boolean -> Boolean -> Array Token -> Markup e
hexes hover grid arr = do
             ul ! className "hexGrid" $ do
               foldMap ( hex hover grid ) ( arrayHex arr )

emptyArrow = { title: "", quantity: 0.0 }

arrayArrow :: Array Token -> Array Flow2
arrayArrow [ _ ]  = ( replicate 10 emptyArrow )
                  <> ( replicate  9 emptyArrow )
                  <> ( replicate 10 emptyArrow )
                  <> ( replicate  3 emptyArrow ) <> singleton  { title: "_", quantity: 2.0 } <> singleton  emptyArrow <> singleton  { title: "_", quantity: 2.0 } <> ( replicate 3 emptyArrow )
                  <> ( replicate 10 emptyArrow )
                  <> ( replicate  9 emptyArrow )

arrayArrow [ _, _ ]  = ( replicate 10 emptyArrow )
                  <> ( replicate  9 emptyArrow )
                  <> ( replicate 10 emptyArrow )
                  <> ( replicate  4 emptyArrow ) <> singleton  { title: "_", quantity: 2.0 } <> ( replicate 4 emptyArrow )
                  <> ( replicate 10 emptyArrow )
                  <> ( replicate  9 emptyArrow )

arrayArrow [ _, _, _ ]  = ( replicate 10 emptyArrow )
                  <> ( replicate  9 emptyArrow )
                  <> ( replicate  4 emptyArrow ) <> singleton  { title: "/", quantity: 2.0 } <> ( replicate 5 emptyArrow )
                  <> ( replicate  4 emptyArrow ) <> singleton  { title: "_", quantity: 2.0 } <> ( replicate 4 emptyArrow )
                  <> ( replicate 10 emptyArrow )
                  <> ( replicate  9 emptyArrow )

--
-- arrayArrow [ a, b, c ]  = ( replicate 10 emptyArrow )
--                      <> ( replicate  4 emptyArrow ) <> singleton c <> ( replicate  4 { title: "" } )
--                      <> ( replicate 10 emptyArrow )
--                      <> ( replicate  3 emptyArrow ) <> singleton a <> ( replicate 1 { title: "" } ) <> singleton b <> ( replicate 3 emptyArrow )
--                      <> ( replicate 10 emptyArrow )
--                      <> ( replicate  9 emptyArrow )
--
-- arrayArrow [ a, b, c, d ]  = ( replicate 10 emptyArrow )
--                         <> ( replicate  4 emptyArrow ) <> singleton c <> ( replicate  4 emptyArrow )
--                         <> ( replicate 10 emptyArrow )
--                         <> ( replicate  3 emptyArrow ) <> singleton a <> ( replicate 1 emptyArrow ) <> singleton b <> ( replicate 3 emptyArrow )
--                         <> ( replicate 10 emptyArrow )
--                         <> ( replicate  4 emptyArrow ) <> singleton d <> ( replicate  4 emptyArrow )

arrayArrow _  = ( replicate 10 emptyArrow )
           <> ( replicate  9 emptyArrow )
           <> ( replicate 10 emptyArrow )
           <> ( replicate  9 emptyArrow )
           <> ( replicate 10 emptyArrow )
           <> ( replicate  9 emptyArrow )

arrows :: forall e. Boolean -> Boolean -> Array Token -> Markup e
arrows hover grid arr = do
             ul ! className "hexGrid flows" $ do
               foldMap flow ( arrayArrow arr )


-- controls ::
tokenList :: forall e. Array Token -> Markup e
tokenList = (ul <<< foldMap (li <<< text <<< _.title ))

optionsTokens Eating = [ { title : "Eating" } ]
optionsTokens EatingBinning = [ { title : "Eating" }, { title: "Binning"} ]
optionsTokens Composting = [ { title : "Eating" }, { title: "Binning"}, {title: "Composting"} ]
optionsTokens CompostingGarden = [ { title : "Eating" }, { title: "Binning"}, {title: "Composting"}, { title: "Garden"} ]
optionsTokens CompostingFoodGarden = [ { title : "Eating" }, { title: "Binning"}, {title: "Composting"}, { title: "Food Garden"} ]
optionsTokens WateringGarden = [ { title : "Garden" }]
optionsTokens RainwaterWateringGarden = [ { title : "Collecting" }, { title : "Garden" }]
optionsTokens NotImplemented = [ { title : "NotImplemented" }]

-- flowsSystem sys =
-- tokenSystem sys =

displayState title available consumed = div ! className "center" $ do
                        text title
                        div ! className "center" $ do
                          text $ ( showAvailable available)
                        div ! className "center" $ do
                          text $ ( showConsumed consumed)
    where
      showAvailable ( Weight _ a ) = "Available : " <> ( show $ trunc a ) <> "kg"
      showAvailable ( Volume _ a ) = "Available : " <> ( show $ trunc a ) <> "L"
      showAvailable ( IncompatibleQuantity ) = "Incompatible Quantity"
      showConsumed ( Weight _ a ) = "Consumed : " <> ( show $ trunc a ) <> "kg"
      showConsumed ( Volume _ a ) = "Consumed : " <> ( show $ trunc a ) <> "L"
      showConsumed ( IncompatibleQuantity ) = "Incompatible Quantity"


interface :: forall e. Boolean -> Boolean -> State -> Markup e
interface hover grid ( State { shoppedFood : ( Stock availableFood consumedFood )
                             , binnedFoodWaste: ( Stock availableBinnedFoodWaste consumedBinnedFoodWaste ) } ) = do
                      displayState "Food: " availableFood consumedFood
                      displayState "FoodWaste: " availableBinnedFoodWaste consumedBinnedFoodWaste
                        -- text $ ( "Binned Food: " <> show ( state.binnedFood ) )
                        -- text $ ( "Managed Waste: " <> show ( state.managedWaste ) )
                      arrows true false $ optionsTokens Eating
                      hexes hover grid $ optionsTokens Eating
                      -- div ! className "center" $ do
                      --   tokenList arr
