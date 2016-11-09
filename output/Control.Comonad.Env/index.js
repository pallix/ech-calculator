// Generated by psc version 0.10.1
"use strict";
var Prelude = require("../Prelude");
var Control_Comonad_Env_Class = require("../Control.Comonad.Env.Class");
var Control_Comonad_Env_Trans = require("../Control.Comonad.Env.Trans");
var Data_Identity = require("../Data.Identity");
var Data_Newtype = require("../Data.Newtype");
var Data_Tuple = require("../Data.Tuple");
var Data_Functor = require("../Data.Functor");
var Data_Function = require("../Data.Function");
var withEnv = Control_Comonad_Env_Trans.withEnvT;
var runEnv = function (v) {
    return Data_Functor.map(Data_Tuple.functorTuple)(Data_Newtype.unwrap(Data_Identity.newtypeIdentity))(v);
};
var mapEnv = Data_Functor.map(Control_Comonad_Env_Trans.functorEnvT(Data_Identity.functorIdentity));
var env = function (e) {
    return function (a) {
        return Control_Comonad_Env_Trans.EnvT(Data_Tuple.Tuple.create(e)(a));
    };
};
module.exports = {
    env: env, 
    mapEnv: mapEnv, 
    runEnv: runEnv, 
    withEnv: withEnv
};
