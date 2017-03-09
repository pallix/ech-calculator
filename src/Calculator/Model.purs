module Calculator.Model (Ratio(..),
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
                         lastState,
                         foldState,
                         foldStateTi,
                         foldNotifications,
                         foldFlows,
                         showQ,
                         Process(..),
                         Matter(..),
                         Entry(..),
                         TimeserieWrapper(..),
                         MatterProperty(..),
                         NotificationType(..),
                         PumpType(..),
                         subQty,
                         addQty,
                         negQty,
                         cappedQty,
                         scaleQty,
                         managingWaste,
                         eating,
                         binning,
                         composting_EatingBinningWormComposting,
                         foodGardening_EatingBinningWormCompostingFoodGardening,
                         foodGardening_EatingBinningWormCompostingFoodGardeningRainwater,
                         rainwaterCollecting_EatingBinningWormCompostingFoodGardenRainwater,
                         eating_EatingBinningWormCompostingFoodSharing,
                         foodSharing,
                         blockToRoofSurface
                        )  where

import Prelude
import Data.Generic
import Calculator.Timeserie (Timeserie)
import Control.Monad.Reader (runReader)
import Data.Array (filter, head, last, mapMaybe, scanl, tail, uncons, (:))
import Data.ArrayBuffer.Types (Int16)
import Data.Date (Date)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Map (Map, delete, empty, insert)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Math (trunc, abs, floor)
import Time (TimeInterval(..), TimePeriod, TimeWindow, dates, defaultTi)

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

-- data Flow a = Flow { input:: a,
--                      output:: a,
--                      stock:: a
--                    }

data SystemParams = SystemParams { houseHoldSize :: Int
                                , estateAveragePersonPerHousehold:: Number
                                , estatePopulation :: Int
                                , estateFlatsOneBedroom :: Int
                                , estateFlatsTwoBedroom :: Int
                                , estateFlatsThreeBedroom :: Int
                                , estateSurfaceArea :: SurfaceArea
                                }

derive instance genericSystemParams :: Generic SystemParams

instance showSystemParams :: Show SystemParams  where
    show = gShow


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
             | RainwaterHarvestingTank
             | RainwaterHarvestingDemand
             | RainwaterHarvestingCollection
             | RainwaterHarvestingDistribution
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
newtype SurfaceArea = SurfaceArea Number

derive instance genericSurfaceArea :: Generic SurfaceArea
derive instance newtypeSurfaceArea :: Newtype SurfaceArea _

instance showSurfaceArea :: Show SurfaceArea  where
    show = gShow

data Scale = PersonScale | HouseholdScale | EstateScale


derive instance genericScale :: Generic Scale

instance showScale :: Show Scale  where
    show = gShow


data Time = Year | Month | Day

type SystemScale = { scale :: Scale,
                     time :: Time, -- TODO: deprecate
                     period :: TimePeriod,
                     window :: TimeWindow}


data Ratio a = Ratio a { ratio :: Number }

derive instance genericRatio :: (Generic a) => Generic (Ratio a)

instance showRatio :: (Show a, Generic a) => Show (Ratio a)  where
    show = gShow

-- model for the event sourcing
data Process =  AllProcess |
                Shopping |
                Eating |
                Binning |
                WormComposting |
                ManagingWaste |
                FoodSharing |
                FoodGardening |
                RoofRainwaterCollecting |
                Living |
                Raining |
                TapWaterSupplying |
                TankRainwaterStoring |
                Cleaning |
                WastewaterCollecting |
                IrrigatingGarden |
                Pumping |
                Debug

derive instance genericProcess :: Generic Process

instance showProcess :: Show Process where
  show = gShow

instance processEq :: Eq Process where
  eq AllProcess _ = true
  eq _ AllProcess = true
  eq a b = gEq a b

instance ordProcess :: Ord Process where
  compare = gCompare


data Matter = AllMatter | Food | Waste | Water | Compost | Fertilizer | GreenhouseGas

derive instance genericMatter :: Generic Matter

instance showMatter :: Show Matter where
  show = gShow

instance matterEq :: Eq Matter where
  eq AllMatter _ = true
  eq _ AllMatter = true
  eq a b = gEq a b

data MatterProperty = Edible | NonEdible | Shopped | Cooked | GreyWater | BlackWater | TapWater | Overflow | Absorbed | AllMatterProperty

derive instance genericMatterProperty :: Generic MatterProperty
instance showMatterProperty :: Show MatterProperty where
  show = gShow

instance matterProperty :: Eq MatterProperty where
  eq AllMatterProperty _ = true
  eq _ AllMatterProperty = true
  eq a b = gEq a b

data NotificationType = TankOverflow | CleaningNotEnoughTankWater | IrrigationGardenNotEnoughTankWater | IrrigationGardenFlowCapacityTooLow | IrrigationGardenFlowCapacityIsZero | CleaningFlowCapacityIsZero | CleaningFlowCapacityTooLow

derive instance genericNotificationType :: Generic NotificationType

instance showNotificatioType :: Show NotificationType  where
    show = gShow

instance eqNotificationTyp :: Eq NotificationType where
  eq = gEq

instance ordNotificationTyp :: Ord NotificationType where
  compare = gCompare

data Entry = Entry { process :: Process
                   , matter :: Matter
                   , matterProperty :: MatterProperty
                   , quantity :: Quantity Matter
                   , interval :: TimeInterval
                   }
           | Notification { process :: Process
                          , typ :: NotificationType
                          , on :: Boolean
                          }
           | Trace { process :: Process
                   , message :: String
                   }
           | Flow { process :: Process
                  , capacity :: Number -- L/m
                  }

derive instance genericEntry :: Generic Entry

instance showEntry :: Show Entry where
  show (Entry { process, matter, matterProperty, quantity }) = "E[ " <> show process <> " " <> show matter <> " " <> show matterProperty <> " " <> show quantity <> "]"
  show (Notification { process, typ, on }) = "N[ " <> show process <> " " <> show typ <> " " <> show on <> "]"
  show (Trace { process, message }) = "T[ " <> show process <> " " <> show message <> "]"
  show (Flow {process, capacity}) = "F[ " <> show process <> " " <> show capacity <> "]"

data State = State (Array Entry)

derive instance genericState :: Generic State

instance showState :: Show State where
    show (State entries) = "[\n" <> strEntries <> "]"
      where strEntries = foldl (\acc e -> acc <> show e <> "\n") "" entries

data Plant = Tomato -- TODO other plants!

hasProcess :: Process -> Entry -> Boolean
hasProcess process (Entry {process: p}) =
  p == process
hasProcess process (Notification {process: p}) =
  p == process
hasProcess process (Trace {process: p}) =
  p == process
hasProcess process (Flow {process: p}) =
  p == process

hasMatter :: Matter -> Entry -> Boolean
hasMatter matter (Entry {matter: m}) =
  m == matter
hasMatter _ (Notification _) = false
hasMatter _ (Trace _) = false
hasMatter _ (Flow _) = false

hasInterval :: TimeInterval -> Entry -> Boolean
hasInterval ti (Entry {interval}) =
  ti == interval
hasInterval _ (Notification _) = false
hasInterval _ (Trace _) = false
hasInterval _ (Flow _) = false

hasMatterProperty :: MatterProperty -> Entry -> Boolean
hasMatterProperty matterProperty (Entry {matterProperty: mp}) =
  mp == matterProperty
hasMatterProperty _ (Notification _) = false
hasMatterProperty _ (Trace _) = false
hasMatterProperty _ (Flow _) = false

foldState :: Process -> Matter -> MatterProperty -> State -> Quantity Matter
foldState process matter matterProperty (State states) = foldl sumQuantity ZeroQuantity quantities
  where
    states' = filter qualifies states
    qualifies = (hasProcess process) && (hasMatter matter) && (hasMatterProperty matterProperty)
    getQuantity (Entry {quantity: q}) = Just q
    getQuantity (Notification _) = Nothing
    getQuantity (Trace _) = Nothing
    getQuantity (Flow _) = Nothing
    quantities = mapMaybe getQuantity states'
    sumQuantity acc qty = acc <> qty

foldStateTi :: Process -> Matter -> MatterProperty -> TimeInterval -> State  -> Quantity Matter
foldStateTi process matter matterProperty ti (State states) = foldl sumQuantity ZeroQuantity quantities
  where
    states' = filter qualifies states
    qualifies = (hasProcess process) && (hasMatter matter) && (hasMatterProperty matterProperty) && (hasInterval ti)
    getQuantity (Entry {quantity: q}) = Just q
    getQuantity (Notification _) = Nothing
    getQuantity (Trace _) = Nothing
    getQuantity (Flow _) = Nothing
    quantities = mapMaybe getQuantity states'
    sumQuantity acc qty = acc <> qty

initialState :: Process -> Matter -> MatterProperty -> State -> Quantity Matter
initialState process matter matterProperty (State states) = maybe ZeroQuantity id $ head quantities
  where
    states' = filter qualifies states
    qualifies = (hasProcess process) && (hasMatter matter) && (hasMatterProperty matterProperty)
    getQuantity (Entry {quantity: q}) = Just q
    getQuantity (Notification _) = Nothing
    getQuantity (Trace _) = Nothing
    getQuantity (Flow _) = Nothing
    quantities = mapMaybe getQuantity states'

lastState :: Process -> Matter -> MatterProperty -> State -> Quantity Matter
lastState process matter matterProperty (State states) = fromMaybe ZeroQuantity $ last quantities
  where
    states' = filter qualifies states
    qualifies = (hasProcess process) && (hasMatter matter) && (hasMatterProperty matterProperty)
    getQuantity (Entry {quantity: q}) = Just q
    getQuantity (Notification _) = Nothing
    getQuantity (Trace _) = Nothing
    getQuantity (Flow _) = Nothing
    quantities = mapMaybe getQuantity states'


foldNotifications :: Process -> State -> Map NotificationType Entry
foldNotifications process (State entries) = foldl f empty entries
  where
    f :: Map NotificationType Entry -> Entry -> Map NotificationType Entry
    f m n@(Notification { typ, on }) = if on then insert typ n m else delete typ m
    f m _ = m

foldFlows :: Process -> State -> Number
foldFlows process (State entries) = foldl f 0.0 entries
  where
    f :: Number -> Entry -> Number
    f m n@(Flow { process, capacity }) = capacity
    f m _ = m

-- /model for the event sourcing

derive instance genericQuantity :: ( Generic a ) => Generic ( Quantity a )
instance showQuantity :: ( Show a ) => Show ( Quantity a ) where
  -- WARNING: introducing a space between the value and the unit breaks the plotting
    show ( Weight _ a ) | a >= 1000000.0 = ( show $ floor $ ( a * 10.0 ) / 10000000.0 ) <> "MTons"
    show ( Volume _ a ) | a >= 1000000.0 = ( show $ floor $ ( a * 10.0 ) / 10000000.0 ) <> "ML"
    show ( Weight _ a ) | a >= 1000.0 = ( show $ trunc ( a * 1.0 ) / 1000.0 ) <> "Tons"
    show ( Volume _ a ) | a >= 1000.0 = ( show $ trunc ( a * 1.0 ) / 1000.0 ) <> "KL"
    show ( Weight _ a ) = ( show $ trunc ( a * 10.0 ) / 10.0 ) <> "Kg"
    show ( Volume _ a ) = ( show $ trunc ( a * 10.0 ) / 10.0 ) <> "L"
    show ( IncompatibleQuantity ) = "IncompatibleQuantity"
    show ( ZeroQuantity ) = "0"

showQ :: forall a. (Show a) => Quantity a -> String
showQ ( Weight _ a ) = show a
showQ ( Volume _ a ) = show a
showQ ( IncompatibleQuantity ) = "IncompatibleQuantity"
showQ ( ZeroQuantity ) = "0"

instance mergeQty :: Semigroup ( Quantity a ) where
  append ( Volume t a ) ( Volume t' b ) = Volume t ( a + b )
  append ( Weight t a ) ( Weight t' b ) = Weight t ( a + b )
  append a ZeroQuantity = a
  append ZeroQuantity b = b
  append _ _ = IncompatibleQuantity


derive instance genericOptions :: Generic Options
instance showOptions :: Show Options where
    show = gShow

data PumpType = KloudKeeper80 | MathModel
derive instance genericPumpType :: Generic PumpType
instance showPumpType :: Show PumpType where
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
                                                   , numberOfBlocks :: Int
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
                                        , timeserieKey :: String
                                        }
                     , tankRainwaterStoringParam :: { title :: String
                                             , surfaceArea :: SurfaceArea
                                             , capacity :: Quantity Matter
                                             }
                     , pumpingParam :: { title :: String
                                       , pumpType :: PumpType
                                       , suctionHead :: Number
                                                        -- ^^ height the water will rise before arriving at the pump (also known as the suction head).
                                       , hydraulicPowerKw :: Number
                                       }
                     , distributingParam :: { title :: String
                                            , dischargeHead :: Number
-- ^^ Maximum Height reached by the pipe after the pump (also known as the 'discharge head').
                                            , surfaceArea :: Number
                                            }
                     , cleaningParam :: { title :: String
                                        , surfaceArea :: SurfaceArea
                                        , waterConsumptionPerSqm :: Number -- TODO TO-ASK
                                        }
                     }

type ProcessParam = Record

data TimeserieWrapper = RainingTimeserie (Timeserie Number) |
                        CleaningTimeserie (Timeserie Number) |
                        IrrigatingGardenTimeserie (Timeserie Number)

data SystemState = SystemState { current :: Options
                               , scale :: SystemScale
                               , state :: State
                               , systemParams :: SystemParams
                               , processParams :: ProcessParams
                               , timeseries :: Map Process TimeserieWrapper
                               , interval :: TimeInterval
                               }


-- derive instance genericSystemState :: Generic SystemState

instance showSystemState :: Show SystemState where
    show (SystemState s) = show s.state -- <> show s.systemParams


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
                           , numberOfBlocks: 1
                              -- liter per sqm (assuming 1000mm of rain, 60% efficency, Rainwater!D20)
                           , collectingCapacity: 0.6
                           }

managedWasteParam = { title: "Managed Waste"
                  , collectedWasteRatio: Ratio Waste  { ratio: 1.0 }
                  , bulkDensity: 0.8 -- kg/L
                  , ghgProduction: 3.0 * 1.3 / 1000.0 -- 3km * 1.3 GHG EF / 1000kg
                  }


rainingParam = { title: "Raining"
               , timeserieKey: "2013" -- TODO currently not used, to implement
               }

tankRainwaterStoringParam = { title: "Storing Rainwater"
                        , surfaceArea: SurfaceArea 4.0 -- cm^2
                        , capacity: Volume Water 1000.0 -- L
                        }

cleaningParam = { title: "Cleaning"
                , surfaceArea: SurfaceArea 200.0
                , waterConsumptionPerSqm: 0.5
                }

pumpingParam = { title: "Pumping"
               , suctionHead: 0.03
               , hydraulicPowerKw: 0.6 -- for the mathematical model only
               , pumpType: KloudKeeper80
               }

distributingParam = { title: "Distributing"
                    , dischargeHead: 30.0
                    , surfaceArea: 10.0
                    }

initProcessParams = { eatingParam
                    , binningParam
                    , wormCompostingParam
                    , managedWasteParam
                    , foodSharingParam
                    , foodGardeningParam
                    , rainwaterCollectingParam
                    , rainingParam
                    , tankRainwaterStoringParam
                    , cleaningParam
                    , pumpingParam
                    , distributingParam
                    }

data Transform a b = Transform a b { ratio :: Number }

derive instance genericTransform :: (Generic a, Generic b) => Generic (Transform a b)

instance showTransform :: (Generic a, Generic b, Show a, Show b) => Show (Transform a b)  where
    show = gShow


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
  [ Entry {process: Shopping, matter: Food, matterProperty: AllMatterProperty, quantity: (negQty shoppedFood), interval: defaultTi}
  , Entry {process: Living, matter: Food, matterProperty: AllMatterProperty, quantity: (eatedFood), interval: defaultTi}
  , Entry {process: Eating, matter: Waste, matterProperty: AllMatterProperty, quantity: wasted, interval: defaultTi}
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
  [ Entry {process: Shopping, matter: Food, matterProperty: AllMatterProperty, quantity: negQty eatedFood, interval: defaultTi}
  , Entry {process: Living, matter: Food, matterProperty: AllMatterProperty, quantity: (eatedFood), interval: defaultTi}
  , Entry {process: Eating, matter: Food, matterProperty: Edible, quantity: edibleWasted, interval: defaultTi}
  , Entry {process: Eating, matter: Waste, matterProperty: NonEdible, quantity: nonedibleWasted, interval: defaultTi}
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
    Entry {process: Eating, matter: Waste, matterProperty: AllMatterProperty, quantity: negQty foodWaste, interval: defaultTi }
  , Entry {process: WormComposting, matter: Waste, matterProperty: AllMatterProperty, quantity: negQty foodWormComposting, interval: defaultTi }
  , Entry {process: Binning, matter: Waste, matterProperty: AllMatterProperty, quantity: allWasteVolume, interval: defaultTi }
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
  [ Entry {process: WormComposting, matter: Compost, matterProperty: AllMatterProperty, quantity: ( negQty usedCompost ), interval: defaultTi }
  , Entry {process: FoodGardening, matter: Fertilizer, matterProperty: AllMatterProperty, quantity: ( subQty fertilizerNeeded usedCompost ), interval: defaultTi}
  , Entry {process: TapWaterSupplying, matter: Water, matterProperty: TapWater, quantity: negQty tapWaterNeeded, interval: defaultTi}
  , Entry {process: FoodGardening, matter: Water, matterProperty: TapWater, quantity: tapWaterNeeded, interval: defaultTi}
  , Entry {process: FoodGardening, matter: Food, matterProperty: Edible, quantity: producedFood, interval: defaultTi}
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
  [ Entry {process: WormComposting, matter: Compost, matterProperty: AllMatterProperty, quantity: negQty usedCompost, interval: defaultTi}
  , Entry {process: RoofRainwaterCollecting, matter: Water, matterProperty: GreyWater, quantity: negQty usedGreyWater, interval: defaultTi}
  , Entry {process: FoodGardening, matter: Water, matterProperty: GreyWater, quantity: usedGreyWater, interval: defaultTi}
  , Entry {process: FoodGardening, matter: Food, matterProperty: Edible, quantity: producedFood, interval: defaultTi}
  ]
  where
    availableCompost = foldState WormComposting Compost AllMatterProperty state
    fertilizerNeeded = case surfaceArea of SurfaceArea area -> mulQty area (case plant of Tomato -> fertilizerNeed.tomato)
    usedCompost = if fertilizerNeeded > availableCompost then
                    availableCompost else subQty availableCompost fertilizerNeeded
    availableGreyWater = foldState RoofRainwaterCollecting Water GreyWater state
    -- surfaceArea' = case (scaleGardenSurface systemScale surfaceArea) of SurfaceArea area -> area
    surfaceArea' = case (surfaceArea) of SurfaceArea area -> area
    greyWaterUsedByPlants = scaleQtyOnTime systemScale $ mulQty surfaceArea' (case plant of Tomato -> greyWaterNeed.tomato)
    greyWaterNeeded = divByRatio greyWaterUsedByPlants irrigationEfficiency
    usedGreyWater = if greyWaterNeeded > availableGreyWater then
                      availableGreyWater else subQty availableGreyWater greyWaterNeeded
    producedFood = mulQty surfaceArea' (case plant of Tomato -> productionCapacity.tomato)


rainwaterCollecting_EatingBinningWormCompostingFoodGardenRainwater :: forall r. ProcessParam (
  numberOfBlocks :: Int,
  collectingCapacity :: Number | r) -> SystemScale -> State -> State
rainwaterCollecting_EatingBinningWormCompostingFoodGardenRainwater {numberOfBlocks,
                                                                    collectingCapacity} systemScale state@(State entries) =
  State $
  entries <>
  [ Entry {process: Raining, matter: Water, matterProperty: GreyWater, quantity: negQty collectedWater, interval: defaultTi}
  , Entry {process: RoofRainwaterCollecting, matter: Water, matterProperty: GreyWater, quantity: collectedWater, interval: defaultTi}
  ]
  where
    rainingWater = foldState Raining Water AllMatterProperty state
    surfaceArea' = blockToRoofSurface numberOfBlocks
    collectedWater = Volume Water $ surfaceArea' * (scaleNumberOnTime systemScale collectingCapacity)

foodSharing :: forall r. ProcessParam (sharedFoodRatio :: Ratio Matter, numberSharingHouseholds :: Int | r ) -> State -> State
foodSharing {sharedFoodRatio, numberSharingHouseholds} state@(State entries) =
  State $
  entries <>
  [ Entry {process: Eating, matter: Waste, matterProperty: AllMatterProperty, quantity: ( negQty edibleShared ), interval: defaultTi}
  , Entry {process: FoodSharing, matter: Food, matterProperty: Edible, quantity: edibleShared, interval: defaultTi}
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
  [ Entry {process: Eating, matter: Waste, matterProperty: AllMatterProperty, quantity: ( negQty compostableWaste ), interval: defaultTi}
  , Entry {process: WormComposting, matter: Waste, matterProperty: AllMatterProperty, quantity: compostableWaste, interval: defaultTi }
  , Entry {process: WormComposting, matter: Compost, matterProperty: AllMatterProperty, quantity: compostProduct, interval: defaultTi }
  , Entry {process: WormComposting, matter: Waste, matterProperty: AllMatterProperty, quantity: negQty compostableWaste, interval: defaultTi }
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
  [ Entry {process: Eating, matter: Waste, matterProperty: AllMatterProperty, quantity: ( negQty foodWaste ), interval: defaultTi}
  , Entry {process: Binning, matter: Waste, matterProperty: AllMatterProperty, quantity: ( negQty binnedWaste ), interval: defaultTi}
  , Entry {process: ManagingWaste, matter: Waste, matterProperty: AllMatterProperty, quantity: allWasteVolume, interval: defaultTi }
  , Entry {process: ManagingWaste, matter: GreenhouseGas, matterProperty: AllMatterProperty, quantity: ghgEmitted, interval: defaultTi }
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

blockToRoofSurface ::  Int -> Number
blockToRoofSurface blocks = (toNumber blocks) * 342.25
