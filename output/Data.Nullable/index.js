// Generated by psc version 0.10.1
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Function = require("../Data.Function");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Data_Maybe = require("../Data.Maybe");
var Data_Show = require("../Data.Show");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Eq = require("../Data.Eq");
var Data_Ord = require("../Data.Ord");
var toNullable = Data_Maybe.maybe($foreign["null"])($foreign.notNull);
var toMaybe = function (n) {
    return $foreign.nullable(n, Data_Maybe.Nothing.value, Data_Maybe.Just.create);
};
var showNullable = function (dictShow) {
    return new Data_Show.Show(function ($3) {
        return Data_Maybe.maybe("null")(Data_Show.show(dictShow))(toMaybe($3));
    });
};
var eqNullable = function (dictEq) {
    return new Data_Eq.Eq(Data_Function.on(Data_Eq.eq(Data_Maybe.eqMaybe(dictEq)))(toMaybe));
};
var ordNullable = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqNullable(dictOrd["__superclass_Data.Eq.Eq_0"]());
    }, Data_Function.on(Data_Ord.compare(Data_Maybe.ordMaybe(dictOrd)))(toMaybe));
};
module.exports = {
    toMaybe: toMaybe, 
    toNullable: toNullable, 
    showNullable: showNullable, 
    eqNullable: eqNullable, 
    ordNullable: ordNullable
};
