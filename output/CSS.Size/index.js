// Generated by psc version 0.10.1
"use strict";
var Prelude = require("../Prelude");
var Data_Generic = require("../Data.Generic");
var CSS_Common = require("../CSS.Common");
var CSS_Property = require("../CSS.Property");
var CSS_String = require("../CSS.String");
var Data_Eq = require("../Data.Eq");
var Data_Ord = require("../Data.Ord");
var Control_Apply = require("../Control.Apply");
var Data_Maybe = require("../Data.Maybe");
var Data_Unit = require("../Data.Unit");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Function = require("../Data.Function");
var Size = function (x) {
    return x;
};
var Angle = function (x) {
    return x;
};
var vw = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("vw"));
};
var vmin = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("vmin"));
};
var vmax = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("vmax"));
};
var vh = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("vh"));
};
var valSize = new CSS_Property.Val(function (v) {
    return v;
});
var valAngle = new CSS_Property.Val(function (v) {
    return v;
});
var sym = function (f) {
    return function (a) {
        return f(a)(a)(a)(a);
    };
};
var rem = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("rem"));
};
var rad = function (i) {
    return Angle(Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("rad")));
};
var px = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("px"));
};
var pt = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("pt"));
};
var pct = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("%"));
};
var nil = Size(CSS_String.fromString(CSS_Property.isStringValue)("0"));
var isStringSize = new CSS_String.IsString(function ($52) {
    return Size(CSS_String.fromString(CSS_Property.isStringValue)($52));
});
var genericSize = function (dictGeneric) {
    return new Data_Generic.Generic(function (v) {
        if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Size.Size" && v.value1.length === 1)) {
            return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Size))(Data_Generic.fromSpine(CSS_Property.genericValue)(v.value1[0](Data_Unit.unit)));
        };
        return Data_Maybe.Nothing.value;
    }, function ($dollarq) {
        return new Data_Generic.SigProd("CSS.Size.Size", [ {
            sigConstructor: "CSS.Size.Size", 
            sigValues: [ function ($dollarq1) {
                return Data_Generic.toSignature(CSS_Property.genericValue)(Data_Generic.anyProxy);
            } ]
        } ]);
    }, function (v) {
        return new Data_Generic.SProd("CSS.Size.Size", [ function ($dollarq) {
            return Data_Generic.toSpine(CSS_Property.genericValue)(v);
        } ]);
    });
};
var genericAngle = function (dictGeneric) {
    return new Data_Generic.Generic(function (v) {
        if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Size.Angle" && v.value1.length === 1)) {
            return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Angle))(Data_Generic.fromSpine(CSS_Property.genericValue)(v.value1[0](Data_Unit.unit)));
        };
        return Data_Maybe.Nothing.value;
    }, function ($dollarq) {
        return new Data_Generic.SigProd("CSS.Size.Angle", [ {
            sigConstructor: "CSS.Size.Angle", 
            sigValues: [ function ($dollarq1) {
                return Data_Generic.toSignature(CSS_Property.genericValue)(Data_Generic.anyProxy);
            } ]
        } ]);
    }, function (v) {
        return new Data_Generic.SProd("CSS.Size.Angle", [ function ($dollarq) {
            return Data_Generic.toSpine(CSS_Property.genericValue)(v);
        } ]);
    });
};
var ex = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("ex"));
};
var eqSize = function (dictEq) {
    return new Data_Eq.Eq(function (x) {
        return function (y) {
            return Data_Eq.eq(CSS_Property.eqValue)(x)(y);
        };
    });
};
var ordSize = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqSize(dictOrd["__superclass_Data.Eq.Eq_0"]());
    }, function (x) {
        return function (y) {
            return Data_Ord.compare(CSS_Property.ordValue)(x)(y);
        };
    });
};
var eqAngle = function (dictEq) {
    return new Data_Eq.Eq(function (x) {
        return function (y) {
            return Data_Eq.eq(CSS_Property.eqValue)(x)(y);
        };
    });
};
var ordAngle = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqAngle(dictOrd["__superclass_Data.Eq.Eq_0"]());
    }, function (x) {
        return function (y) {
            return Data_Ord.compare(CSS_Property.ordValue)(x)(y);
        };
    });
};
var em = function (i) {
    return Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("em"));
};
var deg = function (i) {
    return Angle(Data_Semigroup.append(CSS_Property.semigroupValue)(CSS_Property.value(CSS_Property.valNumber)(i))(CSS_String.fromString(CSS_Property.isStringValue)("deg")));
};
var autoSize = new CSS_Common.Auto(CSS_String.fromString(isStringSize)("auto"));
module.exports = {
    Angle: Angle, 
    Size: Size, 
    deg: deg, 
    em: em, 
    ex: ex, 
    nil: nil, 
    pct: pct, 
    pt: pt, 
    px: px, 
    rad: rad, 
    rem: rem, 
    sym: sym, 
    vh: vh, 
    vmax: vmax, 
    vmin: vmin, 
    vw: vw, 
    eqSize: eqSize, 
    ordSize: ordSize, 
    genericSize: genericSize, 
    isStringSize: isStringSize, 
    valSize: valSize, 
    autoSize: autoSize, 
    eqAngle: eqAngle, 
    ordAngle: ordAngle, 
    genericAngle: genericAngle, 
    valAngle: valAngle
};
