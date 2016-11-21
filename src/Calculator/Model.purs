module Calculator.Model (Token, Flow2, Tok, Flow(Flow), nexusSystem, flowParams, Stock(..), Food(..), Waste(..), Ratio(..), Process(..), Quantity(..), Scale(..), SystemParam(..), Options(..), State(..))  where

import Prelude
-- import Boolean (and)
import Data.Tuple (Tuple(..))
import Data.Generic
-- import Data.Control.Monad (return)


type Tok t = { title :: String | t }

type Token = Tok ()
type Flow2 = Tok ( quantity :: Number )

--
-- Specs / Tests
--
--
-- Given Food & Bin

--   ShoppedFood.output : Food Average Comsumption

---- Food

--   Cooking.input : Food Average Need
--   Cooking.processed: Food Eaten
--   Cooking.output : Food Waste

---- Waste { Food waste, other waste } .

--   Bin.input: Food Waste Produced (Treated/Carried away)

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


--
-- Stocks
--

-- type Food =  { input:: Volume,
--                output:: Volume,
--                stock:: Volume
--              }
--
-- type Waste =  { input:: Volume,
--                 output:: Volume,
--                 stock:: Volume
--               }
--
-- type Water =  { input:: Volume,
--                 output:: Volume,
--                 stock:: Volume
--               }
--
-- type Fertiliser =  { input:: Volume,
--                      output:: Volume,
--                      stock:: Volume
--                    }
--
-- type Life =  { stock:: Volume
--                     }


-- Sources

-- type ShoppedFood =  { output:: Volume }
-- type GardenFood =  { output:: Volume }

-- Sink

-- type WasteManagement =  { input:: Volume,
--                          output:: Volume,
--                          stock:: Volume
--                        }
--
-- type GreenhouseGases =  { input:: Volume,
--                          output:: Volume,
--                          stock:: Volume
--                        }


--
-- Flows
--

-- comsumption is individual level.

-- shopping :: ShoppedFood -> Food
-- shopping shoppedFood = { input: shoppedFood.output, output: 0, stock: 0 }
--
-- harvesting :: GardenFood -> Food
-- harvesting gardenedFood = { input: gardenedFood.output, output: 0, stock: 0 }

data Flow a = Flow { input:: a,
                     output:: a,
                     stock:: a
                   }

-- instance showQuantity :: Show ( Quantity a ) where
--   show ( Volume _ v ) = show v
--   show ( Weight _ w ) = show w
--   show ( IncompatibleQuantity ) = show "Incompatible Quantities"

-- instance showFlow :: Show a => Show ( Flow ( Quantity a ) ) where
--   show ( Flow { input: i, output: o, stock: s } ) = "input: "
--                                                   <> show i
--                                                   <> "\noutput: "
--                                                   <> show o
--                                                   <> "\nstock: "
--                                                   <> show s

data SystemParam = SystemParam { houseHoldSize :: Int
                                , estatePopulation :: Int
                                , estateFlatsOneBedroom :: Int
                                , estateFlatsTwoBedroom :: Int
                                , estateFlatsThreeBedroom :: Int
                                }

data Options = Eating
             | EatingBinning
             | Composting
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

data Food = AnyFood | ShoppedFood | CookedFood | EdibleFoodWaste | SharedFood
data Waste = AnyWaste | FoodWaste | NonEdibleFoodWaste | ManagedWaste


--
-- Living Flows
--

--
-- System
--

-- system :: { output:: Volume } -> { input :: Volume }
-- -- system o = shopping o
-- -- system = shopping >>> eating >>> binning
-- system = eating >>> composting >>> binning

-- Have needs
-- person :: Flow -> Flow -> Life
-- person = unsafeCoerce
--
-- garden :: Flow -> Flow -> Life
-- garden = unsafeCoerce

-- eatingBinning systemP eatingP compostingP binningP input = result

-- eatingBinning :: Scale -> SystemParam -> Param -> Flow -> Flow
-- eatingBinning scale systemP eatingP input = result


-- binningOutput = binning binningP (eatingOutput <> compostingOutput)
-- result = eatingOutput <> compostingOutput <> binningOutput
--

data Life = Life

-- Units

data Quantity a = Weight a Number | Volume a Number | IncompatibleQuantity

-- -- Kg / Person / Day
-- data Weight a =
-- -- L / Person / Day
-- data Volume a =

-- keep track of available and consumed quantities
data Stock a = Stock a a

data Scale = PersonScale | HouseholdScale | EstateScale

data Ratio a = Ratio a { ratio :: Number }

data State = State { shoppedFood :: Stock ( Quantity Food )
                   , cookedFood :: Stock ( Quantity Food )
                   , binnedFoodWaste :: Stock ( Quantity Waste )
                   , sharedFood :: Stock ( Quantity Food )
                   , managedWaste:: Stock ( Quantity Waste )
                   }

derive instance genericFood :: Generic Food
instance showFood :: Show Food where
   show _ = "Food"

derive instance genericWaste :: Generic Waste
instance showWaste :: Show Waste where
   show _ = "Waste"

derive instance genericQuantityFood :: ( Generic a ) => Generic ( Quantity a )
instance showQuantityFood :: ( Show a ) => Show ( Quantity a ) where
    show ( Weight _ a ) = "Weight: " <> show a
    show ( Volume _ a ) = "Volume: " <> show a
    show ( IncompatibleQuantity ) = "IncompatibleQuantity"

derive instance genericStockFood :: Generic ( Stock ( Quantity Food ) )
instance showStockFood :: ( Show a ) => Show ( Stock ( Quantity Food ) ) where
    show ( Stock a b ) = "Available stock: " <> ( show a ) <> "\nConsumed stock: " <> ( show b )

derive instance genericStockWaste :: Generic ( Stock ( Quantity Waste ) )
instance showStockWaste :: ( Show a ) => Show ( Stock ( Quantity Waste ) ) where
    show ( Stock a b ) = "Available stock: " <> ( show a ) <> "\nConsumed stock: " <> ( show b )

derive instance genericState :: Generic State

instance showState :: Show State where
    show = gShow

systemState = State <$> { shoppedFood: Stock ( Weight ShoppedFood 585.0 ) ( Weight ShoppedFood 0.0 )
                        , cookedFood: Stock ( Weight CookedFood 0.0 ) ( Weight CookedFood 0.0 )
                        , binnedFoodWaste: Stock ( Weight FoodWaste 0.0 ) ( Weight FoodWaste 0.0 )
                        , managedWaste: Stock ( Weight ManagedWaste 0.0 ) ( Weight ManagedWaste 0.0 )
                        , sharedFood: _
                        }

type FlowParams = { eatingParam ::
                     { title :: String
                     , eatedFoodRatio :: Ratio Food
                     , allFoodWasteProcess :: Process Food Waste
                     , edibleWasteRatio :: Ratio Waste
                     , nonedibleFoodWasteRatio :: Ratio Waste
                     }
                  , binningParam :: { title :: String
                                    , inputRatio :: Ratio Waste
                                    , allFoodWasteProcess :: Process Food Waste
                      }
                  }

instance mergeQty :: Semigroup ( Quantity a ) where
  append ( Volume t a ) ( Volume t' b ) = Volume t ( a + b )
  append ( Weight t a ) ( Weight t' b ) = Weight t ( a + b )
  append _ _ = IncompatibleQuantity

instance mergeStock :: Semigroup ( Stock ( Quantity Food ) ) where
  append ( Stock qtyAvailable qtyConsumed ) ( Stock qtyAvailable' qtyConsumed' ) = Stock (qtyAvailable <> qtyAvailable') (qtyConsumed' <> qtyConsumed')

instance mergeState :: Semigroup State where
  append s1 s2 = s1
  --  State { shoppedFood : append s1.shoppedFood s2.shoppedFood
  --                      , binnedFoodWaste: s1.binnedFoodWaste <> s2.binnedFoodWaste
  --                      , managedWaste: s1.managedWaste <> s2.managedWaste
  --                      , sharedFood: s1.sharedFood <> s2.sharedFood
  --                      }

type FlowParam = Record

-- systemFlows :: forall r. FlowType -> Record ( title :: String | r )


eatingParam =  { title: "Eating"
               , eatedFoodRatio: Ratio AnyFood { ratio: 0.81 } -- 1 - allFoodWasteRatio
               , allFoodWasteProcess: Process AnyFood AnyWaste { ratio: 0.19 } -- ECH_LCA_Tool:Material Flow Summary!T7 + ECH_LCA_Tool:Material Flow Summary!U7
               , edibleWasteRatio: Ratio AnyWaste { ratio: 0.114 } -- ECH_LCA_Tool:Material Flow Summary!T7
               , nonedibleFoodWasteRatio: Ratio AnyWaste { ratio: 0.076 } -- ECH_LCA_Tool:Material Flow Summary!U7
               }


binningParam = { title: "Binning"
               , inputRatio: Ratio AnyWaste { ratio: 1.0 }
               , allFoodWasteProcess: Process AnyFood AnyWaste { ratio: 0.19 } -- ECH_LCA_Tool:Material Flow Summary!T7 + ECH_LCA_Tool:Material Flow Summary!U7
               }

-- TODO: Maybe this is too cumbersome and should be dealt with when and if we implement this as a EDSL.
--       The idea was to make processes parameters type safe, i.e. making sure that transformations inputs and outputs match.
--       Currently doing this with the process functions (`eating`,...) should be enough.
--

data Process a b = Process a b { ratio :: Number }

applyProcess :: forall a b. Process a b -> Stock ( Quantity a ) -> Stock ( Quantity b )
applyProcess ( Process a b { ratio: ratio }) = updateQty
  where
    updateQty ( Stock ( Weight _ qtyLeft ) ( Weight _ qtyConsumed ) ) =
      Stock ( Weight b (qtyLeft - ( qtyLeft * ratio ) ) ) ( Weight b ( qtyConsumed + (qtyLeft * ratio) ) )
    updateQty ( Stock ( Volume _ qtyLeft ) ( Volume _ qtyConsumed ) ) =
      Stock ( Volume b (qtyLeft - ( qtyLeft * ratio ) ) ) ( Volume b ( qtyConsumed + (qtyLeft * ratio) ) )
    updateQty ( Stock _ _ ) =
      Stock IncompatibleQuantity IncompatibleQuantity


--
-- eatingParam =  { title: "Eating"
--                , eatedFoodProcess: Process ( Food Any ) Life { ratio: 0.81 } -- ( 1 - allFoodWasteProcess
--                , allFoodProcess: Process ( Food Any ) ( Waste FoodWaste ) { ratio: 0.19 } -- ECH_LCA_Tool:Material Flow Summary!T7 + ECH_LCA_Tool:Material Flow Summary!U7
--                , edibleWasteProcess: Process ( Food Any ) ( Food EdibleFoodWaste ) { ratio: 0.114 } -- ECH_LCA_Tool:Material Flow Summary!T7
--                , nonedibleFoodWasteProcess: Process ( Food Any ) ( Waste NonEdibleFoodWaste ) { ratio: 0.076 } -- ECH_LCA_Tool:Material Flow Summary!U7
--                }
--
-- binningParam = { title: "Binning"
--                , inputProcess: Process ( Waste Any ) ManagedWaste { ratio: 1.0 }
--                }

flowParams :: FlowParams
flowParams = { eatingParam : eatingParam
             , binningParam : binningParam
             }

-- systemFlows CompostingFlow = unsafeCoerce
-- systemFlows WateringFlow = unsafeCoerce
-- systemFlows RainwaterCollectingFlow = unsafeCoerce
--

applyRatio :: forall a. Ratio a -> Stock ( Quantity a ) -> Stock ( Quantity a )
applyRatio ( Ratio a { ratio: ratio } ) = updateQty
  where
    updateQty ( Stock ( Weight _ qtyLeft ) ( Weight _ qtyConsumed ) ) =
      Stock ( Weight a (qtyLeft - ( qtyLeft * ratio ) ) ) ( Weight a ( qtyConsumed + (qtyLeft * ratio) ) )
    updateQty ( Stock ( Volume _ qtyLeft ) ( Volume _ qtyConsumed ) ) =
      Stock ( Volume a (qtyLeft - ( qtyLeft * ratio ) ) ) ( Volume a ( qtyConsumed + (qtyLeft * ratio) ) )
    updateQty ( Stock _ _ ) =
      Stock IncompatibleQuantity IncompatibleQuantity

eating :: forall r. FlowParam ( eatedFoodRatio :: Ratio Food | r ) -> State -> State
eating { eatedFoodRatio: eatedFoodRatio } ( State state@{ shoppedFood: shoppedFoodStock } ) = 
  State ( state { shoppedFood = applyRatio eatedFoodRatio shoppedFoodStock } )

binning :: forall r. FlowParam ( allFoodWasteProcess :: Process Food Waste | r ) -> State -> State
binning { allFoodWasteProcess : allFoodWasteProcess } ( State state@{ shoppedFood: shoppedFood } ) =
  State ( state { binnedFoodWaste = applyProcess allFoodWasteProcess shoppedFood } )


composting :: forall r. FlowParam ( r ) -> State -> State
composting _ (State state@{ binnedFoodWaste: waste } ) = State ( state { binnedFoodWaste = waste } )



nexusSystem :: Scale -> SystemParam -> FlowParams -> State -> Options -> State
nexusSystem scale systemP { eatingParam: eatingP } input Eating = eatingOutput
  where
    eatingOutput = eating eatingP input

nexusSystem scale systemP { eatingParam: eatingP, binningParam: binningP } input EatingBinning =
  eatingBinningOutput
  where
    eatingOutput = eating eatingP input
    binningOutput = binning binningP eatingOutput
    eatingBinningOutput = eatingOutput <> binningOutput

nexusSystem scale systemP { eatingParam: eatingP } input Composting = eatingBinningOutput
  where
    eatingOutput = eating eatingP input
    -- compostingOutput = composting compostingP eatingOutput
    -- binningOutput = binning binningP eatingOutput
    eatingBinningOutput = eatingOutput

nexusSystem scale systemP { eatingParam: eatingP } input CompostingGarden = eatingBinningOutput
  where
    eatingOutput = eating eatingP input
    -- compostingOutput = composting compostingP eatingOutput
    -- binningOutput = binning binningP eatingOutput
    eatingBinningOutput = eatingOutput

nexusSystem scale systemP { eatingParam: eatingP } input CompostingFoodGarden = eatingBinningOutput
  where
    eatingOutput = eating eatingP input
    -- compostingOutput = composting compostingP eatingOutput
    -- binningOutput = binning binningP eatingOutput
    eatingBinningOutput = eatingOutput

nexusSystem scale systemP { eatingParam: eatingP } input WateringGarden = eatingBinningOutput
  where
    eatingOutput = eating eatingP input
    -- compostingOutput = composting compostingP eatingOutput
    -- binningOutput = binning binningP eatingOutput
    eatingBinningOutput = eatingOutput

nexusSystem scale systemP { eatingParam: eatingP } input RainwaterWateringGarden = eatingBinningOutput
  where
    eatingOutput = eating eatingP input
    -- compostingOutput = composting compostingP eatingOutput
    -- binningOutput = binning binningP eatingOutput
    eatingBinningOutput = eatingOutput

nexusSystem scale systemP { eatingParam: eatingP } input NotImplemented =
  State { shoppedFood: Stock ( Weight ShoppedFood 0.0 ) ( Weight ShoppedFood 0.0 )
        , cookedFood: Stock ( Weight CookedFood 0.0 ) ( Weight CookedFood 0.0 )
        , binnedFoodWaste: Stock ( Weight FoodWaste 0.0 ) ( Weight FoodWaste 0.0 )
        , managedWaste: Stock ( Weight ManagedWaste 0.0 ) ( Weight ManagedWaste 0.0 )
        , sharedFood: Stock ( Weight SharedFood 0.0 ) ( Weight SharedFood 0.0 )
        }


-- eatingBinning systemP eatingP compostingP binningP input = do
--                         eatingOutput <- eating eatingP input
--                         compostingOutput <- composting compostingP eatingOutput
--                         binningOutput <- binning binningP (eatingOutput <> compostingOutput)
--                         pure eatingOutput <> compostingOutput <> binningOutput
