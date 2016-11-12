"use strict";
var Unsafe_Coerce = require("../Unsafe.Coerce");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Hard = (function () {
    function Hard() {

    };
    Hard.value = new Hard();
    return Hard;
})();
var Medium = (function () {
    function Medium() {

    };
    Medium.value = new Medium();
    return Medium;
})();
var Easy = (function () {
    function Easy() {

    };
    Easy.value = new Easy();
    return Easy;
})();
var Stressful = (function () {
    function Stressful() {

    };
    Stressful.value = new Stressful();
    return Stressful;
})();
var NotStressful = (function () {
    function NotStressful() {

    };
    NotStressful.value = new NotStressful();
    return NotStressful;
})();

/**
 *  Number of individuals
 */
var Scale = (function () {
    function Scale(value0) {
        this.value0 = value0;
    };
    Scale.create = function (value0) {
        return new Scale(value0);
    };
    return Scale;
})();

/**
 *  Qualitative Categories
 */
var LowMaintenance = (function () {
    function LowMaintenance() {

    };
    LowMaintenance.value = new LowMaintenance();
    return LowMaintenance;
})();

/**
 *  Qualitative Categories
 */
var SomeMaintenance = (function () {
    function SomeMaintenance() {

    };
    SomeMaintenance.value = new SomeMaintenance();
    return SomeMaintenance;
})();

/**
 *  Qualitative Categories
 */
var MediumMaintenance = (function () {
    function MediumMaintenance() {

    };
    MediumMaintenance.value = new MediumMaintenance();
    return MediumMaintenance;
})();

/**
 *  Qualitative Categories
 */
var TrainingMaintenance = (function () {
    function TrainingMaintenance() {

    };
    TrainingMaintenance.value = new TrainingMaintenance();
    return TrainingMaintenance;
})();

/**
 *  Qualitative Categories
 */
var HighMaintenance = (function () {
    function HighMaintenance() {

    };
    HighMaintenance.value = new HighMaintenance();
    return HighMaintenance;
})();
var Unhappy = (function () {
    function Unhappy() {

    };
    Unhappy.value = new Unhappy();
    return Unhappy;
})();
var Happy = (function () {
    function Happy() {

    };
    Happy.value = new Happy();
    return Happy;
})();
var Ugly = (function () {
    function Ugly() {

    };
    Ugly.value = new Ugly();
    return Ugly;
})();
var Pretty = (function () {
    function Pretty() {

    };
    Pretty.value = new Pretty();
    return Pretty;
})();

/**
 * 
 *  Flows
 * 
 *  comsumption is individual level.
 */
var shopping = function (shoppedFood) {
    return {
        input: shoppedFood.output, 
        output: 0, 
        stock: 0
    };
};

/**
 *  Have needs
 */
var person = Unsafe_Coerce.unsafeCoerce;

/**
 * 
 *  Specs / Tests
 * 
 * 
 *  Given Food & Bin
 *    ShoppedFood.output : Food Average Comsumption
 * -- Food
 *    Cooking.input : Food Average Need
 *    Cooking.processed: Food Eaten
 *    Cooking.output : Food Waste
 * -- Waste { Food waste, other waste } .
 *    Bin.input: Food Waste Produced (Treated/Carried away)
 * 
 *  Quantities
 * 
 */
var id = function (a) {
    return a;
};
var harvesting = function (gardenedFood) {
    return {
        input: gardenedFood.output, 
        output: 0, 
        stock: 0
    };
};
var garden = Unsafe_Coerce.unsafeCoerce;
var cooking = function (cook) {
    return {
        input: cook.output, 
        output: 0, 
        stock: 0
    };
};
var binning = function (waste) {
    return {
        input: waste.output
    };
};

/**
 * 
 *  Living Flows
 * 
 * 
 *  System
 * 
 *  system o = shopping o
 */
var system = function ($0) {
    return binning(cooking(shopping($0)));
};
module.exports = {
    Ugly: Ugly, 
    Pretty: Pretty, 
    Unhappy: Unhappy, 
    Happy: Happy, 
    LowMaintenance: LowMaintenance, 
    SomeMaintenance: SomeMaintenance, 
    MediumMaintenance: MediumMaintenance, 
    TrainingMaintenance: TrainingMaintenance, 
    HighMaintenance: HighMaintenance, 
    Scale: Scale, 
    Stressful: Stressful, 
    NotStressful: NotStressful, 
    Hard: Hard, 
    Medium: Medium, 
    Easy: Easy, 
    binning: binning, 
    cooking: cooking, 
    garden: garden, 
    harvesting: harvesting, 
    id: id, 
    person: person, 
    shopping: shopping, 
    system: system
};
