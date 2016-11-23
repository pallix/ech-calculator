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

neg :: forall a. Quantity a -> Quantity a
neg (Weight c qty) = Weight c 0.2
neg (Volume c qty) = Volume c 0.5
neg IncompatibleQuantity = IncompatibleQuantity
neg ZeroQuantity = ZeroQuantity

substract :: forall a. Quantity a -> Quantity a -> Quantity a
substract (Weight c qty1) (Weight _ qty2) = Weight c (qty1 - qty2)
substract (Volume c qty1) (Volume _ qty2) = Weight c (qty1 - qty2)
substract q ZeroQuantity = q
substract ZeroQuantity q = neg q
substract _ _ = IncompatibleQuantity

-- -- Kg / Person / Day
-- data Weight a =
-- -- L / Person / Day
-- data Volume a =

data Scale = PersonScale | HouseholdScale | EstateScale

data Ratio a = Ratio a { ratio :: Number }

-- model for the event sourcing
data Process =  AllProcess | Shopping | Eating | Binning | WormComposting

derive instance genericProcess :: Generic Process

instance processEq :: Eq Process where
  eq a b = case [a, b] of
    [Shopping, Shopping] -> true
    [Eating, Eating] -> true
    [Binning, Binning] -> true
    [AllProcess, _] -> true
    [_, AllProcess] -> true
    _ -> false

data Matter = AllMatter | Food | Waste | ManagedWaste | GreyWater | Compost

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


derive instance genericOptions :: Generic Options
instance showOptions :: Show Options where
    show = gShow

type ProcessParams = { eatingParam ::
                          -- missing: packaging quantity
                          { title :: String
                          , eatedFoodRatio :: Ratio Matter
                          , allFoodWasteProcess :: Transform Matter Matter
                          , edibleWasteProcess :: Transform Matter Matter
                          , nonedibleFoodWasteProcess :: Transform Matter Matter
                          }
                     , binningParam :: { title :: String
                                       , inputRatio :: Ratio Matter
                                       , allFoodWasteProcess :: Transform Matter Matter
                                       }
                     , wormCompostingParam :: { title :: String
                                          , inputRatio :: Ratio Matter
                                          , compostProcess :: Transform Matter Matter
                                          }
                     }

type ProcessParam = Record

data SystemState = SystemState { current :: Options, scale :: Scale, state :: State, systemParams :: SystemParams, processParams :: ProcessParams }


eatingParam =  { title: "Eating"
               , eatedFoodRatio: Ratio Food { ratio: 0.81 } -- 1 - allFoodWasteRatio
               , allFoodWasteProcess: Transform Food Waste { ratio: 0.19 } -- ECH_LCA_Tool:Material Flow Summary!T7 + ECH_LCA_Tool:Material Flow Summary!U7
               , edibleWasteProcess: Transform Food Waste { ratio: 0.114 } -- ECH_LCA_Tool:Material Flow Summary!T7
               , nonedibleFoodWasteProcess: Transform Food Waste { ratio: 0.076 } -- ECH_LCA_Tool:Material Flow Summary!U7
               }


binningParam = { title: "Binning"
               , inputRatio: Ratio Waste { ratio: 1.0 }
               , allFoodWasteProcess: Transform Food Waste { ratio: 0.19 } -- ECH_LCA_Tool:Material Flow Summary!T7 + ECH_LCA_Tool:Material Flow Summary!U7
               }

wormCompostingParam = { title: "WormComposting"
                    -- 'Wormery compost'!B9 * 4 (four turnover per year)
                  , compostProcess: Transform Waste Compost { ratio: 0.13 * 4.0 }
                  , inputRatio: Ratio Waste { ratio: 0.6 } -- TODO: it is missing in the sheet, arbitrary number taken
                  }

initProcessParams = { eatingParam
                    , binningParam
                    , wormCompostingParam
                    }

data Transform a b = Transform a b { ratio :: Number }

applyTransform :: Transform Matter Matter -> (Quantity Matter) -> (Quantity Matter)
applyTransform (Transform a b { ratio: r }) = createQuantity
  -- TODO add more typechecks
  where
    createQuantity (Weight _ w) = Weight b $ r * w
    createQuantity (Volume _ w) = Weight b $ r * w
    createQuantity ZeroQuantity = ZeroQuantity
    createQuantity IncompatibleQuantity = IncompatibleQuantity

-- flowParams :: EFlowParams
flowParams = { eatingParam : eatingParam
             , binningParam : binningParam
             }

applyRatio :: forall a. Ratio a -> Quantity a -> Quantity a
applyRatio (Ratio a { ratio: ratio }) qty =
  appRatio ratio qty
  where
    appRatio :: Number -> Quantity a -> Quantity a
    appRatio r (Weight a w) = Weight a $ r * w
    appRatio r (Volume a v) = Volume a $ r * v
    appRatio r ZeroQuantity = ZeroQuantity
    appRatio r IncompatibleQuantity = IncompatibleQuantity

eating :: forall r. ProcessParam ( eatedFoodRatio :: Ratio Matter,
                                edibleWasteProcess :: Transform Matter Matter,
                                nonedibleFoodWasteProcess :: Transform Matter Matter | r ) -> State -> State
eating {eatedFoodRatio: eatedFoodRatio,
        edibleWasteProcess: edibleWasteProcess,
        nonedibleFoodWasteProcess: nonedibleFoodWasteProcess} state@(State entries) =
  State $
  entries <>
  [ Entry {process: Shopping, matter: Food, matterProperty: AllMatterProperty, quantity: neg eated}
  , Entry {process: Eating, matter: Waste, matterProperty: Edible, quantity: edibleWasted}
  , Entry {process: Eating, matter: Waste, matterProperty: NonEdible, quantity: nonedibleWasted}
  ]
  where
    shoppedFood = foldState Shopping Food AllMatterProperty state
    eated = applyRatio eatedFoodRatio shoppedFood
    edibleWasted = applyTransform edibleWasteProcess shoppedFood
    nonedibleWasted = applyTransform nonedibleFoodWasteProcess shoppedFood


binning :: forall r. ProcessParam (allFoodWasteProcess :: Transform Matter Matter | r ) -> State -> State
binning {allFoodWasteProcess: allFoodWasteProcess} state@(State entries) =
  State $
  entries <>
  [
    Entry {process: Eating, matter: Waste, matterProperty: AllMatterProperty, quantity: neg waste}
  , Entry {process: Binning, matter: ManagedWaste, matterProperty: AllMatterProperty, quantity: managed}
  ]
  where
    waste = foldState Eating Waste AllMatterProperty state
    managed = applyTransform allFoodWasteProcess waste

eatingWormComposting :: forall r. ProcessParam (inputRatio :: Ratio Matter,
                                      compostProcess :: Transform Matter Matter | r ) -> State -> State
eatingWormComposting {compostProcess,
                  inputRatio} state@(State entries) =
  State $
  entries <>
  [
    Entry {process: Eating, matter: Waste, matterProperty: AllMatterProperty, quantity: neg wasteInput}
  , Entry {process: WormComposting, matter: Waste, matterProperty: AllMatterProperty, quantity: remaining}
  , Entry {process: WormComposting, matter: Compost, matterProperty: AllMatterProperty, quantity: compost}
  ]
  where
    wastedFood =  foldState Eating Waste AllMatterProperty state
    wasteInput = applyRatio inputRatio wastedFood
    compost = applyTransform compostProcess wasteInput
    remaining = substract wasteInput compost

nexusSystem :: SystemState -> SystemState
-- nexusSystem scale systemP { eatingParam: eatingP } (Tuple option input) = SystemState ( Tuple option
--   case option of
--     Eating -> eating eatingP input
--     Eating -> eatingOutput
--   where
--     eatingOutput =

nexusSystem (SystemState { current, scale, state, systemParams, processParams: processParams@{ eatingParam: eatingP, binningParam: binningP } } ) = SystemState $ { current, scale, systemParams, processParams, state: _ }
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
