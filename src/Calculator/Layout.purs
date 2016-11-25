module Calculator.Layout (interface) where

import Calculator.Model (
  Options(..),
  Process(..),
  Matter(..),
  MatterProperty(..),
  Quantity(..),
  State(State), SystemState(..), foldState, initialState, subQty)

import Math (trunc)
import CSS (darkgrey, Rendered, color, display, renderedSheet, block, render, body, blue, (?), fromString, mediaQuery)
import DOM.Node.Types (documentTypeToNode)
import Data.Array (replicate, singleton)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid (mempty)
import Data.Generic
import Data.Tuple (Tuple(..))
import Text.Smolder.HTML (div, li, ul, table, td, tr, ul, li, p, h2, a, img, style)
import Text.Smolder.HTML.Attributes (lang, charset, httpEquiv, content, name, rel, href, className, src, id)
import Text.Smolder.Markup (on, (#!), Markup, with, text, (!))
import Prelude hiding (div, id)

type Tok t = { title :: String, details :: String | t }

type Token = Tok ()
type Flow2 = Tok ( quantity :: String )

-- example4 :: Rendered
-- example4 = render do
--   body ? do
--     color blue
--   fromString "#world" ? do
--     display block

toCss :: Rendered -> String
toCss r = case renderedSheet r of
              Nothing -> ""
              Just a -> a

-- layout :: String
-- layout = toCss example4

-- css :: forall e. Markup e
-- css = style (text layout)

-- light :: forall e. Boolean -> Markup e
-- light on = div ! arg $ mempty
--   where arg | on = className "on"
--             | otherwise = mempty

hex :: forall e. Boolean -> Boolean -> Token -> Markup e
hex hover grid item = li ! className "hex" ! id item.title $ do
                        tokenToHex item
                      where
                        hoverClass false false = className "hexIn"
                        hoverClass true false = className "hexIn hover"
                        hoverClass _ true = className "hexIn grid"
                        tokenToHex { title: "" } = a ! hoverClass hover grid $ mempty
                        tokenToHex { title } = a ! hoverClass hover grid $ do
                                     img ! src ( image item.title )
                                     h2 $ text title
                                     p $ text item.details
                        -- tokenToHex _  = a ! hoverClass hover grid $ do
                        --              img ! src ( image item.title )
                        image "Eating" = "/images/cooking.svg"
                        image "Binning" = "/images/rubbish_bin.svg"
                        image "Wormery" = "/images/composting.svg"
                        image "Garden" = "https://farm5.staticflickr.com/4144/5053682635_b348b24698.jpg"
                        image "Food Garden" = "/images/food_garden.svg"
                        image "Food Sharing" = "/images/food_sharing.svg"
                        image "Shopped Food" = "/images/shop_food.svg"
                        image "Managed Waste" = "/images/managed_waste.svg"
                        image "Rainwater" = "/images/rainwater.svg"
                        image "Tap Water" = "/images/tapwater.svg"
                        image "Rainwater Collection" = "/images/rainwater_collection.svg"
                        image "_" = "https://dummyimage.com/200x200&text=+"
                        image _ = ""

flow :: forall e. Flow2 -> Markup e
flow item = tokenToHex item
            where
              tokenToHex { title: "_", quantity, details } = li ! className "hex" $ do
                      a ! className "hexIn hover" $ do
                           h2 $ text quantity
                           p $ text details
                           div ! className "arrow-css right" $ mempty
              tokenToHex { title: "/", quantity, details } = li ! className "hex rotate-1" $ do
                      a ! className "hexIn hover" $ do
                           h2 $ text quantity
                           p $ text details
                           div ! className "arrow-css right" $ mempty
                          --  img ! src "https://dummyimage.com/200x200&text=+"
              tokenToHex { title: "\\", quantity, details } = li ! className "hex rotate-2" $ do
                      a ! className "hexIn hover" $ do
                           h2 $ text quantity
                           p $ text details
                           div ! className "arrow-css right" $ mempty
                          --  img ! src "https://dummyimage.com/200x200&text=+"
              tokenToHex _  = li ! className "hex" $ do
                      a ! className "hexIn hover" $ mempty -- $ do
                          --  img ! src "https://dummyimage.com/200x200&text=+"

--
-- optionsTokens CompostingGarden = [ { title : "Eating" }, { title: "Binning"}, {title: "Composting"}, { title: "Garden"} ]
-- optionsTokens CompostingFoodGarden = [ { title : "Eating" }, { title: "Binning"}, {title: "Composting"}, { title: "Food Garden"} ]
-- optionsTokens WateringGarden = [ { title : "Garden" }]
-- optionsTokens RainwaterWateringGarden = [ { title : "Collecting" }, { title : "Garden" }]
-- optionsTokens NotImplemented = [ { title : "NotImplemented" }]

displayState title available consumed = ( showAvailable available) <> " " <> ( showConsumed consumed)
    where
      showAvailable ( Weight _ a ) = "Available : " <> ( show $ trunc a ) <> "kg"
      showAvailable ( Volume _ a ) = "Available : " <> ( show $ trunc a ) <> "L"
      showAvailable ( IncompatibleQuantity ) = "Incompatible Quantity"
      showConsumed ( Weight _ a ) = "Consumed : " <> ( show $ trunc a ) <> "kg"
      showConsumed ( Volume _ a ) = "Consumed : " <> ( show $ trunc a ) <> "L"
      showConsumed ( IncompatibleQuantity ) = "Incompatible Quantity"

emptyHex = { title: "", details: "" }

eatedFood = foldState Living Food AllMatterProperty

gardenFood = foldState FoodGardening Food AllMatterProperty

arrayHex :: SystemState -> Array Token
                      -- displayState "Food: " availableFood consumedFood
                      -- displayState "FoodWaste: " availableBinnedFoodWaste consumedBinnedFoodWaste

arrayHex ( SystemState { current: EatingOnly, state } ) =
                   ( replicate 7 emptyHex )
                <> ( replicate  6 emptyHex )
                <> ( replicate 1 emptyHex ) <> singleton { title : "Shopped Food", details: ( show $ initialShoppedFood state ) <> " of Shopped Food"  }
                                            <> singleton emptyHex
                                            <> singleton { title : "Eating", details: ( show $ eatedFood state ) <> " of Food Consumed" }
                                            <> singleton emptyHex
                                            <> singleton { title : "Managed Waste", details: ( show $ managedWaste state ) <> " of Managed Waste" }
                <> ( replicate 6 emptyHex )
                <> ( replicate  7 emptyHex )

arrayHex ( SystemState { current: EatingBinning, state } ) =
                     ( replicate 7 emptyHex )
                  <> ( replicate  6 emptyHex )
                  <> singleton { title : "Shopped Food", details: "..." }
                  <> singleton emptyHex
                  <> singleton { title : "Eating", details: show $ eatedFood state }
                  <> ( replicate 1 emptyHex )
                  <> singleton { title: "Binning", details: "" }
                  <> singleton emptyHex
                  <> singleton { title : "Managed Waste", details: "..." }
                  <> ( replicate 6 emptyHex )
                  <> ( replicate 7 emptyHex )

arrayHex ( SystemState { current: EatingBinningWormComposting, state } ) =
                     ( replicate 3 emptyHex ) <> singleton { title: "Binning", details: "" }
                                              <> singleton emptyHex <> singleton { title : "Managed Waste", details: "..." }
                                              <> singleton emptyHex
                     <> ( replicate 6 emptyHex )
                     <> singleton { title : "Shopped Food", details: "..." }
                     <> singleton emptyHex
                     <> singleton { title : "Eating", details: show $ eatedFood state }
                     <> ( replicate 1 emptyHex )
                     <> singleton  {title: "Wormery", details: ""}
                     <> singleton emptyHex
                     <> ( replicate 6 emptyHex )
                     <> ( replicate 7 emptyHex )

arrayHex ( SystemState { current: EatingBinningWormCompostingFoodGardening, state } ) =
                     ( replicate 3 emptyHex ) <> singleton  { title: "Binning", details: "" }  <> ( replicate 1 emptyHex ) <> singleton { title : "Managed Waste", details: "..." }  <> ( replicate 1 emptyHex )
                     <> ( replicate 6 emptyHex )
                     <> singleton { title : "Shopped Food", details: "..." }
                     <> singleton emptyHex
                     <> singleton { title : "Eating", details: show $ eatedFood state }
                     <> ( replicate 1 emptyHex )
                     <> singleton {title: "Wormery", details: ""}
                     <> singleton emptyHex
                     <> singleton {title: "Food Garden", details:  show $ gardenFood state}
                     <> ( replicate 6 emptyHex )
                     <> ( replicate 7 emptyHex )

arrayHex ( SystemState { current: EatingBinningWormCompostingFoodGardenWatering, state } ) =
                     ( replicate 3 emptyHex ) <> singleton { title: "Binning", details: "" }  <> ( replicate 1 emptyHex ) <> singleton { title : "Managed Waste", details: "..." }  <> ( replicate 1 emptyHex )
                     <> ( replicate  6 emptyHex )
                     <> singleton { title : "Shopped Food", details: "..." }
                     <> singleton emptyHex
                     <> singleton { title : "Eating", details: show $ eatedFood state }
                     <> ( replicate 1 emptyHex )
                     <> singleton {title: "Wormery", details: ""}
                     <> singleton emptyHex
                     <> singleton {title: "Food Garden", details:  show $ gardenFood state}
                     <> ( replicate 6 emptyHex )
                     <> ( replicate 5 emptyHex ) <> singleton {title: "Tap Water", details: ""}  <> ( replicate 1 emptyHex )

arrayHex ( SystemState { current: EatingBinningWormCompostingFoodGardenRainwater, state } ) =
                     ( replicate 3 emptyHex ) <> singleton { title: "Binning", details: "" }  <> ( replicate 1 emptyHex ) <> singleton { title : "Managed Waste", details: "..." }  <> ( replicate 1 emptyHex )
                     <> ( replicate  6 emptyHex )
                     <> singleton { title : "Shopped Food", details: "..." }
                     <> singleton emptyHex
                     <> singleton { title : "Eating", details: show $ eatedFood state }
                     <> ( replicate 1 emptyHex )
                     <> singleton {title: "Wormery", details: ""}
                     <> singleton emptyHex
                     <> singleton {title: "Food Garden", details:  show $ gardenFood state}
                     <> ( replicate 6 emptyHex )
                     <> ( replicate 3 emptyHex ) <> singleton {title: "Rainwater", details: ""} <> singleton emptyHex <> singleton {title: "Rainwater Collection", details: ""} <> singleton emptyHex

arrayHex ( SystemState { current: EatingBinningFoodSharing, state } ) =
                     ( replicate 3 emptyHex ) <> singleton { title: "Binning", details: "" }  <> ( replicate 1 emptyHex ) <> singleton { title : "Managed Waste", details: "..." }  <> ( replicate 1 emptyHex )
                     <> ( replicate  6 emptyHex )
                     <> singleton { title : "Shopped Food", details: "..." }
                     <> singleton emptyHex
                     <> singleton { title : "Eating", details: show $ eatedFood state }
                     <> ( replicate 4 emptyHex )
                     <> ( replicate 6 emptyHex )
                     <> ( replicate 3 emptyHex ) <> singleton {title: "Food Sharing", details: ""}  <> ( replicate 3 emptyHex )

arrayHex ( SystemState { current: EatingBinningWormCompostingFoodSharing, state } ) =
                     ( replicate 3 emptyHex ) <> singleton { title: "Binning", details: "" }  <> ( replicate 1 emptyHex ) <> singleton { title : "Managed Waste", details: "..." }  <> ( replicate 1 emptyHex )
                     <> ( replicate  6 emptyHex )
                     <> singleton { title : "Shopped Food", details: "..." }
                     <> singleton emptyHex
                     <> singleton { title : "Eating", details: show $ eatedFood state }
                     <> ( replicate 1 emptyHex )
                     <> singleton {title: "Wormery", details: ""}
                     <> singleton emptyHex
                     <> singleton {title: "Food Garden", details:  show $ gardenFood state}
                     <> ( replicate 6 emptyHex )
                     <> ( replicate 3 emptyHex ) <> singleton {title: "Food Sharing", details: ""}  <> ( replicate 3 emptyHex )

--
--
-- arrayHex [ a, b, c, d ]  = ( replicate 10 { title: "" } )
--                         <> ( replicate  4 { title: "" } ) <> singleton c <> ( replicate  4 { title: "" } )
--                         <> ( replicate 10 { title: "" } )
--                         <> ( replicate  3 { title: "" } ) <> singleton a <> ( replicate 1 { title: "" } ) <> singleton b <> ( replicate 3 { title: "" } )
--                         <> ( replicate 10 { title: "" } )
--                         <> ( replicate  4 { title: "" } ) <> singleton d <> ( replicate  4 { title: "" } )

arrayHex _  = ( replicate 7 emptyHex )
           <> ( replicate 6 emptyHex )
           <> ( replicate 7 emptyHex )
           <> ( replicate 6 emptyHex )
           <> ( replicate 7 emptyHex )
           <> ( replicate 6 emptyHex )

hexes :: forall e. Boolean -> Boolean -> SystemState -> Markup e
hexes hover grid (SystemState state@{current}) = do
             ul ! className "hexGrid processes" ! id ( show current ) $ do
               foldMap ( hex hover grid ) ( arrayHex (SystemState state) )

initialShoppedFood = initialState Shopping Food AllMatterProperty
foodWaste = foldState Eating Waste AllMatterProperty

initialBinnedFoodWaste = initialState Eating Waste AllMatterProperty

initialCompostedWaste = initialState WormComposting Waste AllMatterProperty

initialCompost = initialState WormComposting Compost AllMatterProperty

initialFoodShared = initialState FoodSharing Food AllMatterProperty

managedWaste = foldState ManagingWaste Waste AllMatterProperty

emptyArrow = { title: "", quantity: "", details: "" }

arrayArrow :: SystemState -> Array Flow2
arrayArrow (SystemState { current: EatingOnly, state } ) =
                     ( replicate 7 emptyArrow )
                  <> ( replicate 6 emptyArrow )
                  <> ( replicate 2 emptyArrow ) <> singleton { title: "_", quantity: show $ initialShoppedFood state, details: "of Food" }
                                                <> singleton emptyArrow
                                                <> singleton { title: "_", quantity: show $ managedWaste state, details: "of Food Waste" }
                  <> ( replicate 6 emptyArrow )
                  <> ( replicate 7 emptyArrow )

arrayArrow (SystemState { current: EatingBinning, state } ) =
                     ( replicate 7 emptyArrow )
                  <> ( replicate 6 emptyArrow )
                  <> ( replicate 1 emptyArrow ) <> singleton { title: "_", quantity: show $ initialShoppedFood state, details: "of Food" }
                                                 <> singleton emptyArrow
                                                 <> singleton { title: "_", quantity: show $ initialBinnedFoodWaste state, details: "of Food Waste" }
                                                 <> singleton emptyArrow
                                                 <> singleton { title: "_", quantity: show $ managedWaste state, details: "of Waste" }
                  <> ( replicate 6 emptyArrow )
                  <> ( replicate 7 emptyArrow )


arrayArrow (SystemState { current: EatingBinningWormComposting, state } ) =
                     ( replicate 4 emptyArrow ) <> singleton { title: "_", quantity: show $ managedWaste state, details: "of Waste" }
                                                <> ( replicate 2 emptyArrow )
                  <> ( replicate 2 emptyArrow ) <> singleton { title: "/", quantity: show $ ( subQty ( initialBinnedFoodWaste state ) ( initialCompostedWaste state ) ), details: "of Food Waste" }  <> ( replicate 3 emptyArrow )
                  <> ( replicate 1 emptyArrow ) <> singleton { title: "_", quantity: show $ initialShoppedFood state, details: "of Food" }
                                                <> singleton emptyArrow
                                                <> singleton { title: "_", quantity: show $ initialCompostedWaste state , details: "of Compostable Waste " }
                  <> ( replicate 6 emptyArrow )
                  <> ( replicate 7 emptyArrow )


arrayArrow (SystemState { current: EatingBinningWormCompostingFoodGardenWatering, state } ) =
                     ( replicate 4 emptyArrow ) <> singleton { title: "_", quantity: show $ managedWaste state, details: "of Waste" }
                                                <> ( replicate 2 emptyArrow )
                  <> ( replicate 2 emptyArrow ) <> singleton  { title: "/", quantity: show $ ( subQty ( initialBinnedFoodWaste state ) ( initialCompostedWaste state ) ), details: "of Food Waste" } <> ( replicate 3 emptyArrow )
                  <> ( replicate 1 emptyArrow ) <> singleton { title: "_", quantity: show $ initialShoppedFood state, details: "of Food" }
                                                <> singleton emptyArrow
                                                <> singleton { title: "_", quantity: show $ initialCompostedWaste state , details: "of Compostable Waste " }
                                                <> singleton emptyArrow
                                                <> singleton { title: "_", quantity: show $ initialCompost state, details: "of Fertiliser" }
                                                <> ( replicate 1 emptyArrow )
                  <> ( replicate 5 emptyArrow ) <> singleton { title: "/", quantity: show $ initialFoodShared state , details: "of Irrigation Water" } <> ( replicate 3 emptyArrow )
                  <> ( replicate 7 emptyArrow )

arrayArrow (SystemState { current: EatingBinningWormCompostingFoodGardenRainwater, state } ) =
                     ( replicate 4 emptyArrow ) <> singleton { title: "_", quantity: show $ managedWaste state, details: "of Waste" }
                                                <> ( replicate 2 emptyArrow )
                  <> ( replicate 2 emptyArrow ) <> singleton  { title: "/", quantity: show $ ( subQty ( initialBinnedFoodWaste state ) ( initialCompostedWaste state ) ), details: "of Food Waste" } <> ( replicate 3 emptyArrow )
                  <> ( replicate 1 emptyArrow ) <> singleton { title: "_", quantity: show $ initialShoppedFood state, details: "of Food" }
                                                <> singleton emptyArrow
                                                <> singleton { title: "_", quantity: show $ initialCompostedWaste state , details: "of Compostable Waste " }
                                                <> singleton emptyArrow
                                                <> singleton { title: "_", quantity: show $ initialCompost state, details: "of Fertiliser" }
                                                <> ( replicate 1 emptyArrow )
                  <> ( replicate 5 emptyArrow ) <> singleton { title: "/", quantity: show $ initialFoodShared state , details: "of Irrigation Water" } <> ( replicate 1 emptyArrow )
                  <> ( replicate 3 emptyArrow ) <> singleton { title: "_", quantity: show $ initialFoodShared state , details: "of Rainwater" }
                  <> ( replicate 7 emptyArrow )


arrayArrow (SystemState { current: EatingBinningWormCompostingFoodGardening, state } ) =
                     ( replicate 4 emptyArrow ) <> singleton { title: "_", quantity: show $ managedWaste state, details: "of Waste" }
                                                <> ( replicate 2 emptyArrow )
                  <> ( replicate 2 emptyArrow ) <> singleton { title: "/", quantity: show $ ( subQty ( initialBinnedFoodWaste state ) ( initialCompostedWaste state ) ), details: "of Food Waste" } <> ( replicate 3 emptyArrow )
                  <> ( replicate 1 emptyArrow ) <> singleton { title: "_", quantity: show $ initialShoppedFood state, details: "of Food" }
                                                <> singleton emptyArrow
                                                <> singleton { title: "_", quantity: show $ initialCompostedWaste state , details: "of Compostable Waste " }
                                                <> singleton emptyArrow
                                                <> singleton { title: "_", quantity: show $ initialCompost state, details: "of Fertiliser" }
                                                <> ( replicate 2 emptyArrow )
                  <> ( replicate 6 emptyArrow )
                  <> ( replicate 7 emptyArrow )


arrayArrow (SystemState { current: EatingBinningFoodSharing, state } ) =
                     ( replicate 4 emptyArrow ) <> singleton { title: "_", quantity: show $ managedWaste state, details: "of Waste" }
                                                <> ( replicate 2 emptyArrow )
                  <> ( replicate 2 emptyArrow ) <> singleton { title: "/", quantity: show $ ( subQty ( initialBinnedFoodWaste state ) ( initialCompostedWaste state ) ), details: "of Food Waste" } <> ( replicate 3 emptyArrow )
                  <> ( replicate 1 emptyArrow ) <> singleton { title: "_", quantity: show $ initialShoppedFood state, details: "of Food" }
                                                <> singleton emptyArrow
                                                <> ( replicate 4 emptyArrow )
                  <> ( replicate 2 emptyArrow ) <> singleton { title: "\\", quantity: show $ initialFoodShared state , details: "of Food Shared" } <> ( replicate 3 emptyArrow )
                  <> ( replicate 7 emptyArrow )


arrayArrow (SystemState { current: EatingBinningWormCompostingFoodSharing, state } ) =
                     ( replicate 4 emptyArrow ) <> singleton { title: "_", quantity: show $ managedWaste state, details: "of Waste" }
                                                <> ( replicate 2 emptyArrow )
                  <> ( replicate 2 emptyArrow ) <> singleton  { title: "/", quantity: show $ ( subQty ( initialBinnedFoodWaste state ) ( initialCompostedWaste state ) ), details: "of Food Waste" } <> ( replicate 3 emptyArrow )
                  <> ( replicate 1 emptyArrow ) <> singleton { title: "_", quantity: show $ initialShoppedFood state, details: "of Food" }
                                                <> singleton emptyArrow
                                                <> singleton { title: "_", quantity: show $ initialCompostedWaste state , details: "of Compostable Waste " }
                                                <> singleton emptyArrow
                                                <> singleton { title: "_", quantity: show $ initialCompost state, details: "of Fertiliser" }
                                                <> ( replicate 1 emptyArrow )
                  <> ( replicate 2 emptyArrow ) <> singleton { title: "\\", quantity: show $ initialFoodShared state , details: "of Food Shared" } <> ( replicate 3 emptyArrow )
                  <> ( replicate 7 emptyArrow )




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

arrows :: forall e. Boolean -> Boolean -> SystemState -> Markup e
arrows hover grid state = do
             ul ! className "hexGrid flows" $ do
               foldMap flow ( arrayArrow state )


-- controls ::
tokenList :: forall e. Array Token -> Markup e
tokenList = (ul <<< foldMap (li <<< text <<< _.title ))


-- flowsSystem sys =
-- tokenSystem sys =

-- interface :: forall e. Boolean -> Boolean -> SystemState -> Markup e
interface hover grid state = do
                        -- text $ ( "Binned Food: " <> show ( state.binnedFood ) )
                        -- text $ ( "Managed Waste: " <> show ( state.managedWaste ) )
                      arrows true false state
                      hexes hover grid state
                      -- div ! className "center" $ do
                      --   tokenList arr
