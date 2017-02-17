module Calculator.Model (Flow(Flow),
                         nexusSystem,
                         Ratio(..),
                         Transform(..),
                         Quantity(..),
                         SurfaceArea(..),
                         Plant(..),
                         Scale(..),
                         Time(..),
                         SystemScale(..),
                         SystemParams(..),
                         ProcessParams(..),
                         ProcessParam(..),
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
import Data.Generic
import Data.Array (filter, head, tail, uncons, (:))
import Data.ArrayBuffer.Types (Int16)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (maybe, Maybe(..))
import Data.Tuple (Tuple(..))
import Math (trunc, abs)
import Rain (RainfallData, rainfallData)
import Time (TimeResolution, TimeWindow)

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
                                , estateSurfaceArea :: SurfaceArea
                                }

data Options = EatingOnly
             | EatingBinning
             | EatingBinningWormComposting
            --  | EatingBinningWormCompostingGarden
             | EatingBinningWormCompostingFoodGardening
            --  | EatingBinningWormCompostingGardenWatering
             | EatingBinningWormCompostingFoodGardenWatering
            --  | EatingBinningWormCompostingGardenRainwater
             | EatingBinningWormCompostingFoodGardenRainwater
             | EatingBinningFoodSharing
             | EatingBinningWormCompostingFoodSharing
             | NotImplemented

data Life = Life

-- Units

data Quantity a = Weight a Number | Volume a Number | IncompatibleQuantity | ZeroQuantity

instance quantityEq :: Eq (Quantity a) where
  eq (Weight _ w1) (Weight _ w2) = w1 == w2
  eq (Volume _ v1) (Volume _ v2) = v1 == v2
  eq IncompatibleQuantity _ = false
  eq _ IncompatibleQuantity = false
  eq ZeroQuantity ZeroQuantity = true
  eq ZeroQuantity (Weight _ w) = abs (w - 0.0) < 0.0001
  eq (Weight _ w) ZeroQuantity = abs (w - 0.0) < 0.0001
  eq _ _ = false

instance quantityOrd :: Ord (Quantity a) where
  compare (Weight _ w1) (Weight _ w2) = compare w1 w2
  compare (Volume _ v1) (Volume _ v2) = compare v1 v2
  compare ZeroQuantity (Weight _ w) = compare 0.0 w
  compare (Weight _ w) ZeroQuantity = compare w 0.0
  compare (Volume _ v) ZeroQuantity = compare v 0.0
  compare ZeroQuantity (Volume _ v) = compare 0.0 v
  compare ZeroQuantity ZeroQuantity = EQ
  -- IncompatibleQuantity cannot be propagated on a comparison
  compare (Weight _ _) (Volume _ _) = GT
  compare (Volume _ _) (Weight _ _) = LT
  compare IncompatibleQuantity _ = LT
  compare _ IncompatibleQuantity = GT

negQty :: forall a. Quantity a -> Quantity a
negQty (Weight c qty) = Weight c (- qty)
negQty (Volume c qty) = Volume c (- qty)
negQty IncompatibleQuantity = IncompatibleQuantity
negQty ZeroQuantity = ZeroQuantity

subQty :: forall a. Quantity a -> Quantity a -> Quantity a
subQty (Weight c qty1) (Weight _ qty2) = Weight c (qty1 - qty2)
subQty (Volume c qty1) (Volume _ qty2) = Volume c (qty1 - qty2)
subQty q ZeroQuantity = q
subQty ZeroQuantity q = negQty q
subQty _ _ = IncompatibleQuantity

addQty :: forall a. Quantity a -> Quantity a -> Quantity a
addQty (Weight c qty1) (Weight _ qty2) = Weight c (qty1 + qty2)
addQty (Volume c qty1) (Volume _ qty2) = Volume c (qty1 + qty2)
addQty q ZeroQuantity = q
addQty ZeroQuantity q = q
addQty _ _ = IncompatibleQuantity

mulQty :: forall a. Number -> Quantity a -> Quantity a
mulQty n (Weight c qty) = Weight c (n * qty)
mulQty n (Volume c qty) = Volume c (n * qty)
mulQty _ ZeroQuantity = ZeroQuantity
mulQty _ IncompatibleQuantity = IncompatibleQuantity

maxQty :: forall a. Quantity a  -> Quantity a -> Quantity a
maxQty (Weight c qty1) (Weight _ qty2) | qty1 < qty2 = Weight c (qty2)
maxQty (Weight c qty1) (Weight _ qty2) = Weight c (qty1)
maxQty (Volume c qty1) (Volume _ qty2) | qty1 < qty2 = Volume c (qty2)
maxQty (Volume c qty1) (Volume _ qty2) = Volume c (qty1)
maxQty q ZeroQuantity = q
maxQty ZeroQuantity q = q
maxQty _ _ = IncompatibleQuantity

cappedQty :: forall a. Quantity a  -> Quantity a -> Quantity a
cappedQty (Weight c qty1) (Weight _ qty2) | qty1 < qty2 = Weight c (qty1)
cappedQty (Weight c qty1) (Weight _ qty2) = Weight c (qty2)
cappedQty (Volume c qty1) (Volume _ qty2) | qty1 < qty2 = Volume c (qty1)
cappedQty (Volume c qty1) (Volume _ qty2) = Volume c (qty2)
cappedQty q ZeroQuantity = q
cappedQty ZeroQuantity q = q
cappedQty _ _ = IncompatibleQuantity

toVolume :: forall a. Number -> Quantity a -> Quantity a
toVolume bulkDensity (Weight c w) = Volume c $ w / bulkDensity
toVolume _ v@(Volume _ _) = v
toVolume _ ZeroQuantity = ZeroQuantity
toVolume _ IncompatibleQuantity = IncompatibleQuantity

toWeight :: forall a. Number -> Quantity a -> Quantity a
toWeight bulkDensity (Volume c w) = Weight c $ w * bulkDensity
toWeight _ w@(Weight _ _) = w
toWeight _ ZeroQuantity = ZeroQuantity
toWeight _ IncompatibleQuantity = IncompatibleQuantity

-- -- Kg / Person / Day
-- data Weight a =
-- -- L / Person / Day
-- data Volume a =

-- Surface Area in square meters
data SurfaceArea = SurfaceArea Number -- ,,

data Scale = PersonScale | HouseholdScale | EstateScale
data Time = Year | Month | Day

type SystemScale = { scale:: Scale, time:: Time, resolution:: TimeResolution}

data Ratio a = Ratio a { ratio :: Number }

-- model for the event sourcing
data Process =  AllProcess |
                Shopping |
                Eating |
                Binning |
                WormComposting |
                ManagingWaste |
                FoodSharing |
                FoodGardening |
                RainwaterCollecting |
                Living |
                Raining |
                TapWaterSupplying |
                Debug

derive instance genericProcess :: Generic Process

instance showProcess :: Show Process where
  show = gShow

instance processEq :: Eq Process where
  eq AllProcess _ = true
  eq _ AllProcess = true
  eq a b= gEq a b

data Matter = AllMatter | Food | Waste | Water | Compost | Fertilizer | GreenhouseGas

derive instance genericMatter :: Generic Matter

instance showMatter :: Show Matter where
  show = gShow

instance matterEq :: Eq Matter where
  eq AllMatter _ = true
  eq _ AllMatter = true
  eq a b = gEq a b

data MatterProperty = Edible | NonEdible | Shopped | Cooked | GreyWater | TapWater | AllMatterProperty

derive instance genericMatterProperty :: Generic MatterProperty

instance matterProperty :: Eq MatterProperty where
  eq AllMatterProperty _ = true
  eq _ AllMatterProperty = true
  eq a b = gEq a b

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

data Plant = Tomato -- TODO other plants!

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
    show ( Weight _ a ) | a >= 1000.0 = ( show $ trunc ( a * 1.0 ) / 1000.0 ) <> " Tons"
    show ( Weight _ a ) = ( show $ trunc ( a * 10.0 ) / 10.0 ) <> "Kg"
    show ( Volume _ a ) = ( show $ trunc ( a * 10.0 ) / 10.0 ) <> "L"
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
                          , numberHouseholdEating :: Int
                          -- , allFoodWasteProcess :: This is now 1 - eatedFoodRatio
                          , edibleWasteProcess :: Transform Matter Matter
                          -- , nonedibleFoodWasteProcess :: This is now 1 - edibleWasteProcess
                          }
                     , binningParam :: { title :: String
                                       , compactingRatio :: Ratio Matter
                                       , numberCompactors :: Int
                                       , bulkDensity :: Number
                                       }
                     , wormCompostingParam :: { title :: String
                                          , compostableRatio :: Ratio Matter
                                          , numberWormeries :: Int
                                          , compostingYield :: Transform Matter Matter
                                          }
                     , foodSharingParam :: { title :: String
                                           , sharedFoodRatio :: Ratio Matter
                                           , numberSharingHouseholds :: Int
                                           }
                     , foodGardeningParam :: { title :: String
                                             , surfaceArea :: SurfaceArea
                                               -- fertilizer need for one square meter per type of plant
                                             , fertilizerNeed :: { tomato :: Quantity Matter
                                                                 }
                                             , greyWaterNeed :: { tomato :: Quantity Matter
                                                                 }
                                             , plant :: Plant
                                               -- production per square meter
                                             , productionCapacity :: { tomato :: Quantity Matter
                                                                 }
                                             , irrigationEfficiency :: Ratio Matter
                                             }
                     , rainwaterCollectingParam :: { title :: String
                                                   , surfaceArea :: SurfaceArea
                                                     -- L of water collected per square meter
                                                   , collectingCapacity :: Number
                                                   }
                     , managedWasteParam :: { title :: String
                                           , collectedWasteRatio :: Ratio Matter
                                           , bulkDensity :: Number
                                              --  amount of CO2e produced in kg per kg of waste
                                           , ghgProduction :: Number
                                           }
                     , rainingParam ::  { title :: String
                                        , rainfallDataKey :: String
                                        , rainfallData :: RainfallData }

                     }

type ProcessParam = Record

data SystemState = SystemState { current :: Options
                               , scale :: SystemScale
                               , state :: State
                               , systemParams :: SystemParams
                               , processParams :: ProcessParams }


eatingParam =  { title: "Eating"
               , numberHouseholdEating: 121
               , eatedFoodRatio: Ratio Food { ratio: 0.81 } -- 1 - allFoodWasteRatio
              --  , allFoodWasteProcess: Transform Food Waste { ratio: 0.19 } -- ECH_LCA_Tool:Material Flow Summary!T7 + ECH_LCA_Tool:Material Flow Summary!U7
               , edibleWasteProcess: Transform Food Food { ratio: 0.6 } -- ECH_LCA_Tool:Material Flow Summary!T7
              --  , nonedibleFoodWasteProcess: Transform Food Waste { ratio: 0.076 } -- ECH_LCA_Tool:Material Flow Summary!U7
               }

binningParam = { title: "Binning"
               , compactingRatio: Ratio Waste { ratio: 0.30 } -- 0.8 == compactor
              --  , allFoodWasteProcess: Transform Food Waste { ratio: 0.19 } -- ECH_LCA_Tool:Material Flow Summary!T7 + ECH_LCA_Tool:Material Flow Summary!U7
                 -- Compactor!C8:G8
               , numberCompactors: 0
               , bulkDensity: 0.8 -- kg/L
               }

wormCompostingParam = { title: "Wormery"
                    -- 'Wormery compost'!B9 * 4 (four turnover per year)
                  , compostableRatio: Ratio Waste { ratio: 0.6 } -- TODO: it is missing in the sheet, arbitrary number taken
                  , numberWormeries: 0
                  , compostingYield: Transform Waste Compost { ratio: 0.125 * 4.0 }
                  }


foodSharingParam = { title: "Food Sharing"
                  , sharedFoodRatio: Ratio Food { ratio: 0.55 } -- TODO: What is the ratio of available food for sharing to food actually shared?
                  , numberSharingHouseholds: 0
                  }

foodGardeningParam = { title: "Food Garden"
                  , surfaceArea: SurfaceArea 50.0
                    -- 'Material Flow Summary'!O14
                  , fertilizerNeed: { tomato: Weight Fertilizer 2.17
                                    }
                  , greyWaterNeed: { tomato: Volume Water 60.0 -- liter per sqm
                    }
                  , plant: Tomato
                    -- 'Material Flow Summary'!AA14
                  , productionCapacity: { tomato: Weight Food 2.44
                    }
                  , irrigationEfficiency: Ratio Water { ratio: 0.45 }
                  }

rainwaterCollectingParam = { title: "Water collectin"
                           , surfaceArea: SurfaceArea 0.0
                              -- liter per sqm (assuming 1000mm of rain, 60% efficency, Rainwater!D20)
                           , collectingCapacity: 0.6
                           }

managedWasteParam = { title: "Managed Waste"
                  , collectedWasteRatio: Ratio Waste  { ratio: 1.0 }
                  , bulkDensity: 0.8 -- kg/L
                  , ghgProduction: 3.0 * 1.3 / 1000.0 -- 3km * 1.3 GHG EF / 1000kg
                  }


rainingParam = { title: "Raining"
               , rainfallDataKey: "2012"
               , rainfallData: rainfallData}

initProcessParams = { eatingParam
                    , binningParam
                    , wormCompostingParam
                    , managedWasteParam
                    , foodSharingParam
                    , foodGardeningParam
                    , rainwaterCollectingParam
                    , rainingParam
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


divByRatio :: forall a. Quantity a -> Ratio a -> Quantity a
divByRatio qty (Ratio a { ratio: ratio }) =
  divRatio qty ratio
  where
    divRatio :: Quantity a -> Number -> Quantity a
    divRatio (Weight a w) r = Weight a $ w / r
    divRatio (Volume a v) r = Volume a $ v / r
    divRatio ZeroQuantity r = ZeroQuantity -- should be an error here or InfinityQuantity
    divRatio IncompatibleQuantity r = IncompatibleQuantity

complementOneRatio ( Ratio a { ratio } ) = Ratio a { ratio : ( 1.0 - ratio ) }
complementOneTransform ( Transform a b { ratio } ) = Transform a b { ratio : ( 1.0 - ratio ) }
complementOneRatioTransform ( Ratio a { ratio } ) = Transform a a { ratio : ( 1.0 - ratio ) }

eating :: forall r. ProcessParam ( numberHouseholdEating :: Int, eatedFoodRatio :: Ratio Matter | r ) -> State -> State
eating {eatedFoodRatio, numberHouseholdEating} state@(State entries) =
  State $
  entries <>
  [ Entry {process: Shopping, matter: Food, matterProperty: AllMatterProperty, quantity: (negQty shoppedFood)}
  , Entry {process: Living, matter: Food, matterProperty: AllMatterProperty, quantity: (eatedFood)}
  , Entry {process: Eating, matter: Waste, matterProperty: AllMatterProperty, quantity: wasted}
  ]
  where
    shoppedFood = foldState Shopping Food AllMatterProperty state
    eatedFood = rangeScale (toNumber numberHouseholdEating) 121.0 $ applyRatio eatedFoodRatio shoppedFood
    wasted = rangeScale (toNumber numberHouseholdEating) 121.0 $ applyTransform ( complementOneRatioTransform eatedFoodRatio ) shoppedFood

eating_EatingBinningWormCompostingFoodSharing :: forall r. ProcessParam ( eatedFoodRatio :: Ratio Matter,
                                edibleWasteProcess :: Transform Matter Matter | r ) -> State -> State
eating_EatingBinningWormCompostingFoodSharing {eatedFoodRatio, edibleWasteProcess } state@(State entries) =
  State $
  entries <>
  [ Entry {process: Shopping, matter: Food, matterProperty: AllMatterProperty, quantity: negQty eatedFood}
  , Entry {process: Living, matter: Food, matterProperty: AllMatterProperty, quantity: (eatedFood)}
  , Entry {process: Eating, matter: Food, matterProperty: Edible, quantity: edibleWasted}
  , Entry {process: Eating, matter: Waste, matterProperty: NonEdible, quantity: nonedibleWasted}
  ]
  where
    shoppedFood = foldState Shopping Food AllMatterProperty state
    eatedFood = applyRatio eatedFoodRatio shoppedFood
    wasted = applyTransform ( complementOneRatioTransform eatedFoodRatio ) shoppedFood
    edibleWasted = applyTransform edibleWasteProcess wasted
    nonedibleWasted = applyTransform  ( complementOneTransform edibleWasteProcess ) wasted

rangeScale a b (Weight t v)  = Weight t $ ( a / b) * v
rangeScale a b (Volume t v)  = Volume t $ ( a / b) * v
rangeScale _ _ IncompatibleQuantity = IncompatibleQuantity
rangeScale _ _ ZeroQuantity = ZeroQuantity

invertRangeScale a b (Weight t v)  = Weight t $ ( 1.0 - a / b) * v
invertRangeScale a b (Volume t v)  = Volume t $ ( 1.0 - a / b) * v
invertRangeScale _ _ IncompatibleQuantity = IncompatibleQuantity
invertRangeScale _ _ ZeroQuantity = ZeroQuantity

binning :: forall r. ProcessParam ( compactingRatio :: Ratio Matter,
                                    numberCompactors :: Int,
                                    bulkDensity :: Number | r ) -> State -> State
binning {compactingRatio, bulkDensity, numberCompactors} state@(State entries) =
  State $
  entries <>
  [
    Entry {process: Eating, matter: Waste, matterProperty: AllMatterProperty, quantity: negQty foodWaste  }
  , Entry {process: WormComposting, matter: Waste, matterProperty: AllMatterProperty, quantity: negQty foodWormComposting }
  , Entry {process: Binning, matter: Waste, matterProperty: AllMatterProperty, quantity: allWasteVolume }
  ]
  where
    foodWaste = foldState Eating Waste AllMatterProperty state
    foodWormComposting = foldState WormComposting Waste AllMatterProperty state
    allWaste = addQty foodWormComposting foodWaste
    compactedWasteVolume = rangeScale (toNumber numberCompactors) 121.0 $  applyRatio compactingRatio $ toVolume ( bulkDensity ) allWaste
    allWasteVolume = subQty ( toVolume ( bulkDensity ) allWaste) compactedWasteVolume

-- food gardening with tap water
foodGardening_EatingBinningWormCompostingFoodGardening :: forall r. ProcessParam (
  surfaceArea :: SurfaceArea,
  fertilizerNeed :: { tomato :: Quantity Matter },
  greyWaterNeed :: { tomato :: Quantity Matter },
  plant :: Plant,
  productionCapacity :: { tomato :: Quantity Matter},
  irrigationEfficiency :: Ratio Matter | r) -> SystemScale -> State -> State
foodGardening_EatingBinningWormCompostingFoodGardening {surfaceArea,
                                                        fertilizerNeed,
                                                        plant,
                                                        productionCapacity,
                                                        irrigationEfficiency,
                                                        greyWaterNeed} systemScale@{scale, time} state@(State entries) =
  State $
  entries <>
  [ Entry {process: WormComposting, matter: Compost, matterProperty: AllMatterProperty, quantity: ( negQty usedCompost )}
  , Entry {process: FoodGardening, matter: Fertilizer, matterProperty: AllMatterProperty, quantity: ( subQty fertilizerNeeded usedCompost )}
  , Entry {process: TapWaterSupplying, matter: Water, matterProperty: TapWater, quantity: negQty tapWaterNeeded}
  , Entry {process: FoodGardening, matter: Water, matterProperty: TapWater, quantity: tapWaterNeeded}
  , Entry {process: FoodGardening, matter: Food, matterProperty: Edible, quantity: producedFood}
  ]
  where
    availableCompost = foldState WormComposting Compost AllMatterProperty state
    -- surfaceArea' = case (scaleGardenSurface systemScale surfaceArea) of SurfaceArea area -> area
    surfaceArea' = case (surfaceArea) of SurfaceArea area -> area
    fertilizerNeeded = scaleQtyOnTime systemScale $ mulQty surfaceArea' (case plant of Tomato -> fertilizerNeed.tomato)
    usedCompost = if fertilizerNeeded > availableCompost then
                    availableCompost else subQty availableCompost fertilizerNeeded
    availableTapWater = foldState TapWaterSupplying Water TapWater state
    tapWaterUsedByPlants = scaleQtyOnTime systemScale $ mulQty surfaceArea' (case plant of Tomato -> greyWaterNeed.tomato)
    tapWaterNeeded = divByRatio tapWaterUsedByPlants irrigationEfficiency
    -- TODO: in a more accurate model the production should also be a function of the fertilizer
    producedFood = mulQty surfaceArea' (case plant of Tomato -> productionCapacity.tomato)

foodGardening_EatingBinningWormCompostingFoodGardeningRainwater :: forall r. ProcessParam (
  surfaceArea :: SurfaceArea,
  fertilizerNeed :: { tomato :: Quantity Matter },
  greyWaterNeed :: { tomato :: Quantity Matter },
  plant :: Plant,
  productionCapacity :: { tomato :: Quantity Matter},
  irrigationEfficiency :: Ratio Matter | r) -> SystemScale -> State -> State
foodGardening_EatingBinningWormCompostingFoodGardeningRainwater {surfaceArea,
                                                              fertilizerNeed,
                                                              greyWaterNeed,
                                                              plant,
                                                              productionCapacity,
                                                              irrigationEfficiency} systemScale state@(State entries) =
  State $
  entries <>
  [ Entry {process: WormComposting, matter: Compost, matterProperty: AllMatterProperty, quantity: negQty usedCompost}
  , Entry {process: RainwaterCollecting, matter: Water, matterProperty: GreyWater, quantity: negQty usedGreyWater}
  , Entry {process: FoodGardening, matter: Water, matterProperty: GreyWater, quantity: usedGreyWater}
  , Entry {process: FoodGardening, matter: Food, matterProperty: Edible, quantity: producedFood}
  ]
  where
    availableCompost = foldState WormComposting Compost AllMatterProperty state
    fertilizerNeeded = case surfaceArea of SurfaceArea area -> mulQty area (case plant of Tomato -> fertilizerNeed.tomato)
    usedCompost = if fertilizerNeeded > availableCompost then
                    availableCompost else subQty availableCompost fertilizerNeeded
    availableGreyWater = foldState RainwaterCollecting Water GreyWater state
    -- surfaceArea' = case (scaleGardenSurface systemScale surfaceArea) of SurfaceArea area -> area
    surfaceArea' = case (surfaceArea) of SurfaceArea area -> area
    greyWaterUsedByPlants = scaleQtyOnTime systemScale $ mulQty surfaceArea' (case plant of Tomato -> greyWaterNeed.tomato)
    greyWaterNeeded = divByRatio greyWaterUsedByPlants irrigationEfficiency
    usedGreyWater = if greyWaterNeeded > availableGreyWater then
                      availableGreyWater else subQty availableGreyWater greyWaterNeeded
    producedFood = mulQty surfaceArea' (case plant of Tomato -> productionCapacity.tomato)


rainwaterCollecting_EatingBinningWormCompostingFoodGardenRainwater :: forall r. ProcessParam (
  surfaceArea :: SurfaceArea,
  collectingCapacity :: Number | r) -> SystemScale -> State -> State
rainwaterCollecting_EatingBinningWormCompostingFoodGardenRainwater {surfaceArea,
                                                                    collectingCapacity} systemScale state@(State entries) =
  State $
  entries <>
  [ Entry {process: Raining, matter: Water, matterProperty: GreyWater, quantity: negQty collectedWater}
  , Entry {process: RainwaterCollecting, matter: Water, matterProperty: GreyWater, quantity: collectedWater}
  ]
  where
    rainingWater = foldState Raining Water AllMatterProperty state
    surfaceArea' = blockToRoofSurface surfaceArea
    collectedWater = Volume Water $ surfaceArea' * (scaleNumberOnTime systemScale collectingCapacity)

foodSharing :: forall r. ProcessParam (sharedFoodRatio :: Ratio Matter, numberSharingHouseholds :: Int | r ) -> State -> State
foodSharing {sharedFoodRatio, numberSharingHouseholds} state@(State entries) =
  State $
  entries <>
  [ Entry {process: Eating, matter: Waste, matterProperty: AllMatterProperty, quantity: ( negQty edibleShared )}
  , Entry {process: FoodSharing, matter: Food, matterProperty: Edible, quantity: edibleShared}
  ]
  where
    edibleWasted = foldState Eating Food Edible state
    edibleShared = rangeScale (toNumber numberSharingHouseholds) 121.0 $ applyRatio sharedFoodRatio edibleWasted

composting_EatingBinningWormComposting :: forall r. ProcessParam (compostableRatio :: Ratio Matter,
                                      compostingYield :: Transform Matter Matter
                                      , numberWormeries :: Int | r ) -> SystemScale -> State -> State
composting_EatingBinningWormComposting {compostingYield
                                       ,numberWormeries
                                       ,compostableRatio} systemScale state@(State entries) =
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
    -- 100L wormeries. i.e. 80kg
    wormeryCapacity = applyRatio ( Ratio Compost { ratio: toNumber(numberWormeries) } ) ( Weight Compost 80.0 )
    compostProduct = applyTransform compostingYield $ (scaleQtyOnTime systemScale $ cappedQty wormeryCapacity compostableWaste)
    -- compostWaste = subQty compostableWaste compostProduct

managingWaste :: forall r. ProcessParam (collectedWasteRatio :: Ratio Matter,
                                         bulkDensity :: Number,
                                         ghgProduction :: Number | r ) -> State -> State
managingWaste {collectedWasteRatio,
               bulkDensity,
               ghgProduction} state@(State entries) =
  State $
  entries <>
  [ Entry {process: Eating, matter: Waste, matterProperty: AllMatterProperty, quantity: ( negQty foodWaste )}
  , Entry {process: Binning, matter: Waste, matterProperty: AllMatterProperty, quantity: ( negQty binnedWaste )}
  , Entry {process: ManagingWaste, matter: Waste, matterProperty: AllMatterProperty, quantity: allWasteVolume }
  , Entry {process: ManagingWaste, matter: GreenhouseGas, matterProperty: AllMatterProperty, quantity: ghgEmitted }
  -- , Entry {process: WormComposting, matter: Waste, matterProperty: AllMatterProperty, quantity: compostWaste }
  ]
  where
    foodWaste =  foldState Eating Waste AllMatterProperty state
    binnedWaste =  foldState Binning Waste AllMatterProperty state
    allWasteVolume = addQty (toVolume bulkDensity foodWaste) (toVolume bulkDensity binnedWaste)
    ghgEmitted = mulQty ghgProduction allWasteVolume

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

scaleQtyOnTime :: forall a. SystemScale -> Quantity a -> Quantity a
scaleQtyOnTime {time} q =
  scaleQ q
  where
    timeFactor = case time of
      Day -> (1.0 / 365.25)
      Month -> (1.0 / 12.0)
      Year -> 1.0
    applyFactor qty = timeFactor * qty
    scaleQ (Weight a qty) = Weight a (applyFactor qty)
    scaleQ (Volume a qty) = Volume a (applyFactor qty)
    scaleQ ZeroQuantity = ZeroQuantity
    scaleQ IncompatibleQuantity = IncompatibleQuantity

scaleNumberOnTime :: forall a. SystemScale -> Number -> Number
scaleNumberOnTime {time} n =
  timeFactor * n
  where
    timeFactor = case time of
      Day -> (1.0 / 365.25)
      Month -> (1.0 / 12.0)
      Year -> 1.0


scaleGardenSurface :: SystemScale -> SurfaceArea -> SurfaceArea
scaleGardenSurface {scale} _ =
  case scale of
    PersonScale -> SurfaceArea 10.0
    HouseholdScale -> SurfaceArea 10.0
    EstateScale -> SurfaceArea 100.0

blockToRoofSurface ::  SurfaceArea -> Number
blockToRoofSurface (SurfaceArea blocks ) = blocks * 342.25

scaleFirstEntry :: SystemScale -> SystemParams -> State -> State
scaleFirstEntry systemScale systemParams (State entries) =
  State $ case uncons entries of
    Nothing -> []
    Just {head: h,
          tail: xs} -> case h of Entry entry@{quantity} -> (Entry $ entry { quantity = scaleQty systemScale systemParams quantity }) : xs

nexusSystem :: SystemState -> SystemState
nexusSystem (SystemState sys@{ current, scale, state, systemParams, processParams: processParams } ) = SystemState $ sys { state = endState }
  where
    state' = scaleFirstEntry scale systemParams state
    endState = case current of
      EatingOnly -> managingWaste processParams.managedWasteParam
                  $ eating processParams.eatingParam state'
      EatingBinning -> managingWaste processParams.managedWasteParam
                     $ binning processParams.binningParam
                     $ eating processParams.eatingParam state'
      EatingBinningWormComposting -> managingWaste processParams.managedWasteParam
                                   $ binning processParams.binningParam
                                   $ composting_EatingBinningWormComposting processParams.wormCompostingParam scale
                                   $ eating processParams.eatingParam state'
      EatingBinningWormCompostingFoodGardening -> managingWaste processParams.managedWasteParam
                                   $ binning processParams.binningParam
                                   $ foodGardening_EatingBinningWormCompostingFoodGardening processParams.foodGardeningParam scale
                                   $ composting_EatingBinningWormComposting processParams.wormCompostingParam scale
                                   $ eating processParams.eatingParam state'
      EatingBinningWormCompostingFoodGardenWatering -> managingWaste processParams.managedWasteParam
                                   $ binning processParams.binningParam
                                   $ foodGardening_EatingBinningWormCompostingFoodGardening processParams.foodGardeningParam scale
                                   $ composting_EatingBinningWormComposting processParams.wormCompostingParam scale
                                   $ eating processParams.eatingParam state'
      EatingBinningWormCompostingFoodGardenRainwater -> managingWaste processParams.managedWasteParam
                                   $ binning processParams.binningParam
                                   $ foodGardening_EatingBinningWormCompostingFoodGardeningRainwater processParams.foodGardeningParam scale
                                   $ rainwaterCollecting_EatingBinningWormCompostingFoodGardenRainwater processParams.rainwaterCollectingParam scale
                                   $ composting_EatingBinningWormComposting processParams.wormCompostingParam scale
                                   $ eating processParams.eatingParam state'
      EatingBinningFoodSharing -> managingWaste processParams.managedWasteParam
                                 $ binning processParams.binningParam
                                 -- TODO replug on eating?
                                 -- $ eating ...
                                 $ foodSharing processParams.foodSharingParam
                                 $ eating_EatingBinningWormCompostingFoodSharing processParams.eatingParam state'
      EatingBinningWormCompostingFoodSharing -> managingWaste processParams.managedWasteParam
                                   $ binning processParams.binningParam
                                   $ composting_EatingBinningWormComposting processParams.wormCompostingParam scale
                                   -- TODO replug on eating?
                                   -- $ eating ...
                                   $ foodSharing processParams.foodSharingParam
                                   $ eating_EatingBinningWormCompostingFoodSharing processParams.eatingParam state'

      _ -> State []
