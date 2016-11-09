// Generated by psc version 0.10.1
"use strict";
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Plus = require("../Control.Plus");
var Data_Foldable = require("../Data.Foldable");
var Data_List_Lazy = require("../Data.List.Lazy");
var Data_Monoid = require("../Data.Monoid");
var Data_Newtype = require("../Data.Newtype");
var Data_Traversable = require("../Data.Traversable");
var Partial_Unsafe = require("../Partial.Unsafe");
var Data_Show = require("../Data.Show");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Eq = require("../Data.Eq");
var Data_Ord = require("../Data.Ord");
var Data_Functor = require("../Data.Functor");
var Control_Apply = require("../Control.Apply");
var Data_Function = require("../Data.Function");
var Control_Applicative = require("../Control.Applicative");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Bind = require("../Control.Bind");
var Data_List_Lazy_Types = require("../Data.List.Lazy.Types");
var ZipList = function (x) {
    return x;
};
var traversableZipList = Data_List_Lazy_Types.traversableList;
var showZipList = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(ZipList " + (Data_Show.show(Data_List_Lazy_Types.showList(dictShow))(v) + ")");
    });
};
var semigroupZipList = Data_List_Lazy_Types.semigroupList;
var ordZipList = function (dictOrd) {
    return Data_List_Lazy_Types.ordList(dictOrd);
};
var newtypeZipList = new Data_Newtype.Newtype(function (n) {
    return n;
}, ZipList);
var monoidZipList = Data_List_Lazy_Types.monoidList;
var functorZipList = Data_List_Lazy_Types.functorList;
var foldableZipList = Data_List_Lazy_Types.foldableList;
var eqZipList = function (dictEq) {
    return Data_List_Lazy_Types.eqList(dictEq);
};
var applyZipList = new Control_Apply.Apply(function () {
    return functorZipList;
}, function (v) {
    return function (v1) {
        return Data_List_Lazy.zipWith(Data_Function.apply)(v)(v1);
    };
});
var zipListIsNotBind = function (dictFail) {
    return new Control_Bind.Bind(function () {
        return applyZipList;
    }, Partial_Unsafe.unsafeCrashWith("bind: unreachable"));
};
var applicativeZipList = new Control_Applicative.Applicative(function () {
    return applyZipList;
}, function ($13) {
    return ZipList(Data_List_Lazy.repeat($13));
});
var altZipList = new Control_Alt.Alt(function () {
    return functorZipList;
}, Data_Semigroup.append(semigroupZipList));
var plusZipList = new Control_Plus.Plus(function () {
    return altZipList;
}, Data_Monoid.mempty(monoidZipList));
var alternativeZipList = new Control_Alternative.Alternative(function () {
    return applicativeZipList;
}, function () {
    return plusZipList;
});
module.exports = {
    ZipList: ZipList, 
    showZipList: showZipList, 
    newtypeZipList: newtypeZipList, 
    eqZipList: eqZipList, 
    ordZipList: ordZipList, 
    semigroupZipList: semigroupZipList, 
    monoidZipList: monoidZipList, 
    foldableZipList: foldableZipList, 
    traversableZipList: traversableZipList, 
    functorZipList: functorZipList, 
    applyZipList: applyZipList, 
    applicativeZipList: applicativeZipList, 
    altZipList: altZipList, 
    plusZipList: plusZipList, 
    alternativeZipList: alternativeZipList, 
    zipListIsNotBind: zipListIsNotBind
};
