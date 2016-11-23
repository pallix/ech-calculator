module Calculator.Model (Flow(Flow),
                         nexusSystem,
                         Ratio(..),
                         Transform(..),
                         Quantity(..),
                         Scale(..),
                         SystemParams(..),
                         ProcessParams(..),
                         initProcessParams,
                         Options(..),
                         State(..),
                         SystemState(..),
                         initState,
                         foldState,
                         Process(..),
                         Matter(..),
                         Entry(..),
                         MatterProperty(..)
                        )  where

import Prelude
-- import Boolean (and)
import Data.Tuple (Tuple(..))
import Data.Generic
import Data.Foldable (foldl)
import Data.Array (filter, head, tail)
import Data.Maybe (Maybe(..))

--
-- Quantities
--

id :: forall a. a -> a
id a = a

-- Qualitative Categories

data Maintenability = LowMaintenance | SomeMaintenance | MediumMaintenance | TrainingMaintenance | HighMaintenance

data Aesthetic = Ugly | Pretty
data Usability = Hard | Medium | Easy

data Happinness = Unhappy | Happy
data Stress = Stressful | NotStressful

data Flow a = Flow { input:: a,
                     output:: a,
                     stock:: a
                   }

data SystemParams = SystemParams { houseHoldSize :: Int
                                , estatePopulation :: Int
                                , estateFlatsOneBedroom :: Int
                                , estateFlatsTwoBedroom :: Int
                                , estateFlatsThreeBedroom :: Int
                                }

data Options = EatingOnly
             | EatingBinning
             | CompostingOnly
             | CompostingGarden
             | CompostingFoodGarden
             | WateringGarden
             | RainwaterWateringGarden
             | NotImplemented

data FlowType = EatingFlow
              | BinningFlow
              | CompostingFlow
              | WateringFlow
              | RainwaterCollectingFlow

data Life = Life

-- Units

data Quantity a = Weight a Number | Volume a Number | IncompatibleQuantity | ZeroQuantity

-- -- Kg / Person / Day
-- data Weight a =
-- -- L / Person / Day
-- data Volume a =

data Scale = PersonScale | HouseholdScale | EstateScale

data Ratio a = Ratio a { ratio :: Number }

-- model for the event sourcing
data Process = Shopping | Eating | Binning | AllProcess

derive instance genericProcess :: Generic Process

instance processEq :: Eq Process where
  eq a b = case [a, b] of
    [Shopping, Shopping] -> true
    [Eating, Eating] -> true
    [Binning, Binning] -> true
    [AllProcess, _] -> true
    [_, AllProcess] -> true
    _ -> false

data Matter = Food | Waste | ManagedWaste | AllMatter

derive instance genericMatter :: Generic Matter

instance showMatter :: Show Matter where
  show = gShow

instance matterEq :: Eq Matter where
  eq a b = case [a, b] of
    [Food, Food] -> true
    [Waste, Waste] -> true
    [AllMatter, _] -> true
    [_, AllMatter] -> true
    _ -> false

data MatterProperty = Edible | NonEdible | Shopped | Cooked | AllMatterProperty

derive instance genericMatterProperty :: Generic MatterProperty

instance matterProperty :: Eq MatterProperty where
  eq a b = case [a, b] of
    [Edible, Edible] -> true
    [NonEdible, NonEdible] -> true
    [Shopped, Shopped] -> true
    [Cooked, Cooked] -> true
    [AllMatterProperty, _] -> true
    [_, AllMatterProperty] -> true
    _ -> false

data Entry = Entry { process :: Process
                   , matter :: Matter
                   , matterProperty :: MatterProperty
                   , quantity :: Quantity Matter
                   }

derive instance genericEntry :: Generic Entry

instance showEntry :: Show Entry where
  show = gShow

data State = State (Array Entry)

initState = State [ Entry {process: Shopping, matter: Food, matterProperty: Shopped, quantity: Weight Food 120.0}
                  , Entry {process: Shopping, matter: Food, matterProperty: Shopped, quantity: Weight Food (-20.0)}
                  , Entry {process: Eating, matter: Waste, matterProperty: NonEdible, quantity: Weight Waste 10.0} ]


derive instance genericState :: Generic State

instance showState :: Show State where
    show = gShow

hasProcess :: Process -> Entry -> Boolean
hasProcess process (Entry {process: p}) =
  p == process

hasMatter :: Matter -> Entry -> Boolean
hasMatter matter (Entry {matter: m}) =
  m == matter

hasMatterProperty :: MatterProperty -> Entry -> Boolean
hasMatterProperty matterProperty (Entry {matterProperty: mp}) =
  mp == matterProperty

foldState :: Process -> Matter -> MatterProperty -> State -> Quantity Matter
foldState process matter matterProperty (State states) = foldl sumQuantity ZeroQuantity quantities
  where
    states' = filter qualifies states
    qualifies = (hasProcess process) && (hasMatter matter) && (hasMatterProperty matterProperty)
    getQuantity (Entry {quantity: q}) = q
    quantities = map getQuantity states'
    sumQuantity acc qty = acc <> qty

-- /model for the event sourcing

derive instance genericQuantity :: ( Generic a ) => Generic ( Quantity a )
instance showQuantity :: ( Show a ) => Show ( Quantity a ) where
    show ( Weight _ a ) = "Weight: " <> show a
    show ( Volume _ a ) = "Volume: " <> show a
    show ( IncompatibleQuantity ) = "IncompatibleQuantity"
    show ( ZeroQuantity ) = "0"

instance mergeQty :: Semigroup ( Quantity a ) where
  append ( Volume t a ) ( Volume t' b ) = Volume t ( a + b )
  append ( Weight t a ) ( Weight t' b ) = Weight t ( a + b )
  append a ZeroQuantity = a
  append ZeroQuantity b = b
  append _ _ = IncompatibleQuantity

type ProcessParams = { eatingParam :: { title :: String
                                       , eatedFoodRatio :: Ratio Matter
                                       , allFoodWasteProcess :: Transform Matter Matter
                                       , edibleWasteRatio :: Ratio Matter
                                       , nonedibleFoodWasteRatio :: Ratio Matter
                                       }
                      , binningParam :: { title :: String
                                        , inputRatio :: Ratio Matter
                                        , allFoodWasteProcess :: Transform Matter Matter
                                        }
                      }


type ProcessParam = Record


data SystemState = SystemState { current :: Options, scale :: Scale, state :: State, systemParams :: SystemParams, processParams :: ProcessParams }

-- systemFlows :: forall r. FlowType -> Record ( title :: String | r )


eatingParam =  { title: "Eating"
               , eatedFoodRatio: Ratio Food { ratio: 0.81 } -- 1 - allFoodWasteRatio
               , allFoodWasteProcess: Transform Food Waste { ratio: 0.19 } -- ECH_LCA_Tool:Material Flow Summary!T7 + ECH_LCA_Tool:Material Flow Summary!U7
               , edibleWasteRatio: Ratio Food { ratio: 0.114 } -- ECH_LCA_Tool:Material Flow Summary!T7
               , nonedibleFoodWasteRatio: Ratio Waste { ratio: 0.076 } -- ECH_LCA_Tool:Material Flow Summary!U7
               }


binningParam = { title: "Binning"
               , inputRatio: Ratio Waste { ratio: 1.0 }
               , allFoodWasteProcess: Transform Food Waste { ratio: 0.19 } -- ECH_LCA_Tool:Material Flow Summary!T7 + ECH_LCA_Tool:Material Flow Summary!U7
               }

initProcessParams = { eatingParam, binningParam }

-- TODO: Maybe this is too cumbersome and should be dealt with when and if we implement this as a EDSL.
--       The idea was to make processes parameters type safe, i.e. making sure that transformations inputs and outputs match.
--       Currently doing this with the process functions (`eating`,...) should be enough.
--

data Transform a b = Transform a b { ratio :: Number }

-- applyProcess :: forall a b. Process a b -> Stock ( Quantity a ) -> Stock ( Quantity b )
-- applyProcess (Process a b { ratio: ratio }) = updateQty
--   where
--     updateQty ( Stock ( Weight _ qtyLeft ) ( Weight _ qtyConsumed ) ) =
--       Stock ( Weight b (qtyLeft - ( qtyLeft * ratio ) ) ) ( Weight b ( qtyConsumed + (qtyLeft * ratio) ) )
--     updateQty ( Stock ( Volume _ qtyLeft ) ( Volume _ qtyConsumed ) ) =
--       Stock ( Volume b (qtyLeft - ( qtyLeft * ratio ) ) ) ( Volume b ( qtyConsumed + (qtyLeft * ratio) ) )
--     updateQty ( Stock _ _ ) =
--       Stock IncompatibleQuantity IncompatibleQuantity

applyTransform :: Transform Matter Matter -> (Quantity Matter) -> (Quantity Matter)
applyTransform (Transform a b { ratio: r }) = createQuantity
  -- TODO add more typechecks
  where
    createQuantity (Weight _ w) = Weight b $ r * w
    createQuantity (Volume _ w) = Weight b $ r * w
    createQuantity ZeroQuantity = ZeroQuantity
    createQuantity IncompatibleQuantity = IncompatibleQuantity

processParams = { eatingParam : eatingParam
             , binningParam : binningParam
             }

applyRatio :: forall a. Ratio a -> Quantity a -> Quantity a
applyRatio (Ratio a { ratio: ratio }) qty =
  appRatio ratio qty
  where
    appRatio :: Number -> Quantity a -> Quantity a
    appRatio r (Weight a w) = Weight a $ - (r * w)
    appRatio r (Volume a v) = Volume a $ - (r * v)
    appRatio r ZeroQuantity = ZeroQuantity
    appRatio r IncompatibleQuantity = IncompatibleQuantity

-- eating :: forall r. FlowParam ( eatedFoodRatio :: Ratio Food | r ) -> State -> State
-- eating { eatedFoodRatio: eatedFoodRatio } ( State state@{ shoppedFood: shoppedFoodStock } ) =
--   State ( state { shoppedFood = applyRatio eatedFoodRatio shoppedFoodStock,
--                   binnedFoodWaste = Stock (Weight AnyWaste 68.0) (Weight AnyWaste 2.0)} )

eating :: forall r. ProcessParam ( eatedFoodRatio :: Ratio Matter,
                                 allFoodWasteProcess :: Transform Matter Matter | r ) -> State -> State
eating {eatedFoodRatio: eatedFoodRatio,
         allFoodWasteProcess: allFoodWasteProcess} state@(State entries) =
  State $
  entries <>
  [ Entry {process: Shopping, matter: Food, matterProperty: AllMatterProperty, quantity: consumed}
  , Entry {process: Eating, matter: Waste, matterProperty: NonEdible, quantity: wasted}
  ]
  where
    shoppedFood = foldState Shopping Food AllMatterProperty state
    consumed = applyRatio eatedFoodRatio shoppedFood
    wasted = applyTransform allFoodWasteProcess shoppedFood


binning :: forall r. ProcessParam (allFoodWasteProcess :: Transform Matter Matter | r ) -> State -> State
binning {allFoodWasteProcess: allFoodWasteProcess} state@(State entries) =
  State $
  entries <>
  [
    -- TODO: issue an entry to remove EWaste
    Entry {process: Binning, matter: ManagedWaste, matterProperty: NonEdible, quantity: managed}
  ]
  where
    waste = foldState Eating Waste AllMatterProperty state
    managed = applyTransform allFoodWasteProcess waste


-- binning :: forall r. FlowParam ( allFoodWasteProcess :: Process Food Waste | r ) -> State -> State
-- binning { allFoodWasteProcess : allFoodWasteProcess } ( State state@{ shoppedFood: shoppedFood } ) =
--   State ( state { binnedFoodWaste = applyProcess allFoodWasteProcess shoppedFood
--                   --    Stock (Weight AnyWaste 66.0) (Weight AnyWaste 66.0),
--                   -- shoppedFood =
--                   --   Stock (Weight AnyFood 66.0) (Weight AnyFood 66.0)
--                 } )

-- composting :: forall r. FlowParam ( r ) -> State -> State
-- composting _ (State state@{ binnedFoodWaste: waste } ) = State ( state { binnedFoodWaste = waste } )

nexusSystem :: SystemState -> SystemState
-- nexusSystem scale systemP { eatingParam: eatingP } (Tuple option input) = SystemState ( Tuple option
--   case option of
--     Eating -> eating eatingP input
--     Eating -> eatingOutput
--   where
--     eatingOutput =

nexusSystem (SystemState { current, scale, state, systemParams, processParams: { eatingParam: eatingP, binningParam: binningP } } ) = SystemState $ { current, scale, systemParams, processParams, state: _ }
  case current of
    EatingOnly -> eating eatingP state
    _ -> State []

-- nexusSystem scale systemP { eatingParam: eatingP, binningParam: binningP } (SystemState ( Tuple EatingBinning input ) ) = SystemState $ Tuple EatingBinning binningOutput
--   where
--     eatingOutput = eating eatingP state
--     binningOutput = binning binningP eatingOutput
--     -- eatingBinningOutput = eatingOutput -- <> binningOutput
--
-- nexusSystem scale systemP { eatingParam: eatingP } (SystemState ( Tuple CompostingOnly input ) ) = SystemState $ Tuple CompostingOnly eatingBinningOutput
--   where
--     eatingOutput = eating eatingP input
--     -- compostingOutput = composting compostingP eatingOutput
--     -- binningOutput = binning binningP eatingOutput
--     eatingBinningOutput = eatingOutput
--
-- nexusSystem scale systemP { eatingParam: eatingP } (SystemState ( Tuple CompostingGarden input ) ) = SystemState $ Tuple CompostingGarden eatingBinningOutput
--   where
--     eatingOutput = eating eatingP input
--     -- compostingOutput = composting compostingP eatingOutput
--     -- binningOutput = binning binningP eatingOutput
--     eatingBinningOutput = eatingOutput
--
-- nexusSystem scale systemP { eatingParam: eatingP } (SystemState ( Tuple CompostingFoodGarden input ) ) = SystemState $ Tuple CompostingFoodGarden eatingBinningOutput
--   where
--     eatingOutput = eating eatingP input
--     -- compostingOutput = composting compostingP eatingOutput
--     -- binningOutput = binning binningP eatingOutput
--     eatingBinningOutput = eatingOutput
--
-- nexusSystem scale systemP { eatingParam: eatingP } (SystemState ( Tuple WateringGarden input ) ) = SystemState $ Tuple WateringGarden eatingBinningOutput
--   where
--     eatingOutput = eating eatingP input
--     -- compostingOutput = composting compostingP eatingOutput
--     -- binningOutput = binning binningP eatingOutput
--     eatingBinningOutput = eatingOutput
--
-- nexusSystem scale systemP { eatingParam: eatingP } (SystemState ( Tuple RainwaterWateringGarden input ) ) = SystemState $ Tuple RainwaterWateringGarden eatingBinningOutput
--   where
--     eatingOutput = eating eatingP input
--     -- compostingOutput = composting compostingP eatingOutput
--     -- binningOutput = binning binningP eatingOutput
--     eatingBinningOutput = eatingOutput
--
-- nexusSystem scale systemP { eatingParam: eatingP } (SystemState ( Tuple NotImplemented input ) ) = SystemState $ Tuple NotImplemented $ State []


-- eatingBinning systemP eatingP compostingP binningP input = do
--                         eatingOutput <- eating eatingP input
--                         compostingOutput <- composting compostingP eatingOutput
--                         binningOutput <- binning binningP (eatingOutput <> compostingOutput)
--                         pure eatingOutput <> compostingOutput <> binningOutput
