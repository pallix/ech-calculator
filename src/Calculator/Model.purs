module Calculator.Model (Flow(Flow),
                         nexusSystem,
                         Ratio(..),
                         Transform(..),
                         Quantity(..),
                         Scale(..),
                         Time(..),
                         SystemScale(..),
                         SystemParams(..),
                         ProcessParams(..),
                         initProcessParams,
                         Options(..),
                         State(..),
                         SystemState(..),
                         initialState,
                         foldState,
                         Process(..),
                         Matter(..),
                         Entry(..),
                         MatterProperty(..),
                         subQty,
                         addQty
                        )  where

import Prelude
-- import Boolean (and)
import Data.Tuple (Tuple(..))
import Data.Generic
import Data.Foldable (foldl)
import Data.Array (filter, head, tail)
import Data.Maybe (maybe, Maybe(..))
import Data.Int (toNumber)
import Math (trunc)

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
                                , estateAveragePersonPerHousehold:: Number
                                , estatePopulation :: Int
                                , estateFlatsOneBedroom :: Int
                                , estateFlatsTwoBedroom :: Int
                                , estateFlatsThreeBedroom :: Int
                                }

data Options = EatingOnly
             | EatingBinning
             | EatingBinningWormComposting
             | EatingBinningWormCompostingGarden
             | EatingBinningWormCompostingFoodGarden
             | EatingBinningWormCompostingGardenWatering
             | EatingBinningWormCompostingFoodGardenWatering
             | EatingBinningWormCompostingGardenRainwater
             | EatingBinningWormCompostingFoodGardenRainwater
             | EatingBinningWormCompostingFoodSharing
             | NotImplemented

data Life = Life

-- Units

data Quantity a = Weight a Number | Volume a Number | IncompatibleQuantity | ZeroQuantity

negQty :: forall a. Quantity a -> Quantity a
negQty (Weight c qty) = Weight c (- qty)
negQty (Volume c qty) = Volume c (- qty)
negQty IncompatibleQuantity = IncompatibleQuantity
negQty ZeroQuantity = ZeroQuantity

subQty :: forall a. Quantity a -> Quantity a -> Quantity a
subQty (Weight c qty1) (Weight _ qty2) = Weight c (qty1 - qty2)
subQty (Volume c qty1) (Volume _ qty2) = Weight c (qty1 - qty2)
subQty q ZeroQuantity = q
subQty ZeroQuantity q = negQty q
subQty _ _ = IncompatibleQuantity

addQty :: forall a. Quantity a -> Quantity a -> Quantity a
addQty (Weight c qty1) (Weight _ qty2) = Weight c (qty1 + qty2)
addQty (Volume c qty1) (Volume _ qty2) = Weight c (qty1 + qty2)
addQty q ZeroQuantity = q
addQty ZeroQuantity q = q
addQty _ _ = IncompatibleQuantity

-- -- Kg / Person / Day
-- data Weight a =
-- -- L / Person / Day
-- data Volume a =

data Scale = PersonScale | HouseholdScale | EstateScale
data Time = Year | Month | Day

type SystemScale = { scale:: Scale, time:: Time}

data Ratio a = Ratio a { ratio :: Number }

-- model for the event sourcing
data Process =  AllProcess | Shopping | Eating | Binning | WormComposting | ManagingWaste

derive instance genericProcess :: Generic Process

instance processEq :: Eq Process where
  eq a b = case [a, b] of
    [Shopping, Shopping] -> true
    [Eating, Eating] -> true
    [Binning, Binning] -> true
    [ManagingWaste, ManagingWaste] -> true
    [WormComposting, WormComposting] -> true
    [AllProcess, _] -> true
    [_, AllProcess] -> true
    _ -> false

data Matter = AllMatter | Food | Waste | GreyWater | Compost

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

initialState :: Process -> Matter -> MatterProperty -> State -> Quantity Matter
initialState process matter matterProperty (State states) = maybe ZeroQuantity id $ head quantities
  where
    states' = filter qualifies states
    qualifies = (hasProcess process) && (hasMatter matter) && (hasMatterProperty matterProperty)
    getQuantity (Entry {quantity: q}) = q
    quantities = map getQuantity states'

-- /model for the event sourcing

derive instance genericQuantity :: ( Generic a ) => Generic ( Quantity a )
instance showQuantity :: ( Show a ) => Show ( Quantity a ) where
    show ( Weight _ a ) = ( show $ trunc a ) <> "Kg"
    show ( Volume _ a ) = ( show $ trunc a ) <> "L"
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
                          -- , allFoodWasteProcess :: This is now 1 - eatedFoodRatio
                          , edibleWasteProcess :: Transform Matter Matter
                          -- , nonedibleFoodWasteProcess :: This is now 1 - edibleWasteProcess
                          }
                     , binningParam :: { title :: String
                                       , compactingRatio :: Ratio Matter
                                       }
                     , wormCompostingParam :: { title :: String
                                          , compostableRatio :: Ratio Matter
                                          , compostingYield :: Transform Matter Matter
                                          }
                     , foodSharingParam :: { title :: String
                                           , sharedFoodRatio :: Ratio Matter
                                           }
                     , managedWasteParam :: { title :: String
                                           , collectedWasteRatio :: Ratio Matter
                                           }

                     }

type ProcessParam = Record

data SystemState = SystemState { current :: Options
                               , scale :: SystemScale
                               , state :: State
                               , systemParams :: SystemParams
                               , processParams :: ProcessParams }


eatingParam =  { title: "Eating"
               , eatedFoodRatio: Ratio Food { ratio: 0.81 } -- 1 - allFoodWasteRatio
              --  , allFoodWasteProcess: Transform Food Waste { ratio: 0.19 } -- ECH_LCA_Tool:Material Flow Summary!T7 + ECH_LCA_Tool:Material Flow Summary!U7
               , edibleWasteProcess: Transform Food Food { ratio: 0.6 } -- ECH_LCA_Tool:Material Flow Summary!T7
              --  , nonedibleFoodWasteProcess: Transform Food Waste { ratio: 0.076 } -- ECH_LCA_Tool:Material Flow Summary!U7
               }

binningParam = { title: "Binning"
               , compactingRatio: Ratio Waste { ratio: 0.0 }
              --  , allFoodWasteProcess: Transform Food Waste { ratio: 0.19 } -- ECH_LCA_Tool:Material Flow Summary!T7 + ECH_LCA_Tool:Material Flow Summary!U7
               }

wormCompostingParam = { title: "Wormery"
                    -- 'Wormery compost'!B9 * 4 (four turnover per year)
                  , compostableRatio: Ratio Waste { ratio: 0.6 } -- TODO: it is missing in the sheet, arbitrary number taken
                  , compostingYield: Transform Waste Compost { ratio: 0.125 * 4.0 }
                  }


foodSharingParam = { title: "Food Sharing"
                  , sharedFoodRatio: Ratio Food { ratio: 1.0 } -- TODO: What is the ratio of available food for sharing to food actually shared?
                  }

managedWasteParam = { title: "Managed Waste"
                  , collectedWasteRatio: Ratio Waste  { ratio: 1.0 }
                  }

initProcessParams = { eatingParam
                    , binningParam
                    , wormCompostingParam
                    , managedWasteParam
                    , foodSharingParam
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

applyRatio :: forall a. Ratio a -> Quantity a -> Quantity a
applyRatio (Ratio a { ratio: ratio }) qty =
  appRatio ratio qty
  where
    appRatio :: Number -> Quantity a -> Quantity a
    appRatio r (Weight a w) = Weight a $ r * w
    appRatio r (Volume a v) = Volume a $ r * v
    appRatio r ZeroQuantity = ZeroQuantity
    appRatio r IncompatibleQuantity = IncompatibleQuantity

complementOneRatio ( Ratio a { ratio } ) = Ratio a { ratio : ( 1.0 - ratio ) }
complementOneTransform ( Transform a b { ratio } ) = Transform a b { ratio : ( 1.0 - ratio ) }
complementOneRatioTransform ( Ratio a { ratio } ) = Transform a a { ratio : ( 1.0 - ratio ) }

eating :: forall r. ProcessParam ( eatedFoodRatio :: Ratio Matter | r ) -> State -> State
eating {eatedFoodRatio} state@(State entries) =
  State $
  entries <>
  [ Entry {process: Shopping, matter: Food, matterProperty: AllMatterProperty, quantity: (negQty shoppedFood)}
  , Entry {process: Eating, matter: Food, matterProperty: AllMatterProperty, quantity: (eatedFood)}
  , Entry {process: Eating, matter: Waste, matterProperty: AllMatterProperty, quantity: wasted}
  ]
  where
    shoppedFood = foldState Shopping Food AllMatterProperty state
    eatedFood = applyRatio eatedFoodRatio shoppedFood
    wasted = applyTransform ( complementOneRatioTransform eatedFoodRatio ) shoppedFood

binning :: forall r. ProcessParam ( r ) -> State -> State
binning _ state@(State entries) =
  State $
  entries <>
  [
    Entry {process: Eating, matter: Waste, matterProperty: AllMatterProperty, quantity: negQty foodWaste  }
  , Entry {process: WormComposting, matter: Waste, matterProperty: AllMatterProperty, quantity: negQty foodWormComposting }
  , Entry {process: Binning, matter: Waste, matterProperty: AllMatterProperty, quantity: ( addQty foodWaste foodWormComposting ) }
  ]
  where
    foodWaste = foldState Eating Waste AllMatterProperty state
    foodWormComposting = foldState WormComposting Waste AllMatterProperty state

composting_EatingBinningWormComposting :: forall r. ProcessParam (compostableRatio :: Ratio Matter,
                                      compostingYield :: Transform Matter Matter | r ) -> State -> State
composting_EatingBinningWormComposting {compostingYield,
                  compostableRatio} state@(State entries) =
  State $
  entries <>
  [ Entry {process: Eating, matter: Waste, matterProperty: AllMatterProperty, quantity: ( negQty compostableWaste )}
  , Entry {process: WormComposting, matter: Waste, matterProperty: AllMatterProperty, quantity: compostableWaste }
  , Entry {process: WormComposting, matter: Compost, matterProperty: AllMatterProperty, quantity: compostProduct }
  , Entry {process: WormComposting, matter: Waste, matterProperty: AllMatterProperty, quantity: negQty compostableWaste }
  -- , Entry {process: WormComposting, matter: Waste, matterProperty: AllMatterProperty, quantity: compostWaste }
  ]
  where
    wastedFood =  foldState Eating Waste AllMatterProperty state
    compostableWaste = applyRatio compostableRatio wastedFood
    compostProduct = applyTransform compostingYield compostableWaste
    -- compostWaste = subQty compostableWaste compostProduct

managingWaste  :: forall r. ProcessParam (collectedWasteRatio :: Ratio Matter | r ) -> State -> State
managingWaste {collectedWasteRatio} state@(State entries) =
  State $
  entries <>
  [ Entry {process: Eating, matter: Waste, matterProperty: AllMatterProperty, quantity: ( negQty foodWaste )}
  , Entry {process: Binning, matter: Waste, matterProperty: AllMatterProperty, quantity: ( negQty binnedWaste )}
  , Entry {process: ManagingWaste, matter: Waste, matterProperty: AllMatterProperty, quantity: ( addQty binnedWaste foodWaste ) }
  -- , Entry {process: WormComposting, matter: Waste, matterProperty: AllMatterProperty, quantity: compostWaste }
  ]
  where
    foodWaste =  foldState Eating Waste AllMatterProperty state
    binnedWaste =  foldState Binning Waste AllMatterProperty state

eating_EatingBinningWormCompostingFoodSharing :: forall r. ProcessParam ( eatedFoodRatio :: Ratio Matter,
                                edibleWasteProcess :: Transform Matter Matter | r ) -> State -> State
eating_EatingBinningWormCompostingFoodSharing {eatedFoodRatio, edibleWasteProcess } state@(State entries) =
  State $
  entries <>
  [ Entry {process: Shopping, matter: Food, matterProperty: AllMatterProperty, quantity: negQty eatedFood}
  , Entry {process: Eating, matter: Food, matterProperty: AllMatterProperty, quantity: (eatedFood)}
  , Entry {process: Eating, matter: Food, matterProperty: Edible, quantity: edibleWasted}
  , Entry {process: Eating, matter: Waste, matterProperty: NonEdible, quantity: nonedibleWasted}
  ]
  where
    shoppedFood = foldState Shopping Food AllMatterProperty state
    eatedFood = applyRatio eatedFoodRatio shoppedFood
    wasted = applyTransform ( complementOneRatioTransform eatedFoodRatio ) shoppedFood
    edibleWasted = applyTransform edibleWasteProcess wasted
    nonedibleWasted = applyTransform  ( complementOneTransform edibleWasteProcess ) shoppedFood

scaleQty :: forall a. SystemScale -> SystemParams -> Quantity a -> Quantity a
scaleQty {scale, time} (SystemParams {estateAveragePersonPerHousehold, estatePopulation}) q =
  scaleQ q
  where scaleFactor = case scale of
          PersonScale -> 1.0
          HouseholdScale -> estateAveragePersonPerHousehold
          EstateScale -> toNumber estatePopulation
        timeFactor = case time of
          Day -> (1.0 / 365.25)
          Month -> (1.0 / 12.0)
          Year -> 1.0
        applyFactors qty = scaleFactor * timeFactor * qty
        scaleQ (Weight a qty) = Weight a (applyFactors qty)
        scaleQ (Volume a qty) = Volume a (applyFactors qty)
        scaleQ ZeroQuantity = ZeroQuantity
        scaleQ IncompatibleQuantity = IncompatibleQuantity

scaleEntries :: SystemScale -> SystemParams -> State -> State
scaleEntries systemScale systemParams (State entries) =
  State $ scaledEntries
  where
        convertEntries (Entry entry@{quantity}) = Entry $ entry { quantity = scaleQty systemScale systemParams quantity }
        scaledEntries = map convertEntries entries

nexusSystem :: SystemState -> SystemState
nexusSystem (SystemState sys@{ current, scale, state, systemParams, processParams: processParams } ) = SystemState $ sys { state = endState }
  where
    state' = scaleEntries scale systemParams state
    endState = case current of
      EatingOnly -> managingWaste processParams.managedWasteParam
                  $ eating processParams.eatingParam state'
      EatingBinning -> managingWaste processParams.managedWasteParam
                     $ binning processParams.binningParam
                     $ eating processParams.eatingParam state'
      EatingBinningWormComposting -> managingWaste processParams.managedWasteParam
                                   $ binning processParams.binningParam
                                   $ composting_EatingBinningWormComposting processParams.wormCompostingParam
                                   $ eating processParams.eatingParam state'
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
