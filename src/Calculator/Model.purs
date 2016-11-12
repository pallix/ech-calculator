module Calculator.Model (system)  where

import Unsafe.Coerce (unsafeCoerce)
import Control.Semigroupoid ((>>>))

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

-- Kg / Day
type Volume = Int

-- Qualitative Categories

data Maintenability = LowMaintenance | SomeMaintenance | MediumMaintenance | TrainingMaintenance | HighMaintenance

data Aesthetic = Ugly | Pretty
data Usability = Hard | Medium | Easy

data Happinness = Unhappy | Happy
data Stress = Stressful | NotStressful

--
-- Stocks
--

type Food =  { input:: Volume,
               output:: Volume,
               stock:: Volume
             }

type Waste =  { input:: Volume,
                output:: Volume,
                stock:: Volume
              }

type Water =  { input:: Volume,
                output:: Volume,
                stock:: Volume
              }

type Fertiliser =  { stock:: Volume
                    }

type Life =  { stock:: Volume
                    }


-- Sources

type ShoppedFood =  { output:: Volume }
type GardenFood =  { output:: Volume }

-- Sink

type WasteManagement =  { input :: Volume }
type GreenhouseGases =  { input :: Volume }

--
-- Flows
--

-- comsumption is individual level.

shopping :: ShoppedFood -> Food
shopping shoppedFood = { input: shoppedFood.output, output: 0, stock: 0 }

harvesting :: GardenFood -> Food
harvesting gardenedFood = { input: gardenedFood.output, output: 0, stock: 0 }

cooking :: Food -> Waste
cooking cook = { input: cook.output, output: 0, stock: 0 }

binning :: Waste -> WasteManagement
binning waste = { input: waste.output }

--
-- Living Flows
--

--
-- System
--

system :: { output:: Volume } -> { input :: Volume }
-- system o = shopping o
system = shopping >>> cooking >>> binning

-- Have needs
person :: Food -> Water -> Life
person = unsafeCoerce

garden :: Water -> Fertiliser -> Life
garden = unsafeCoerce

-- Number of individuals
data Scale = Scale Int
