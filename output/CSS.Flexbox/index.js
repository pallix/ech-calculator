// Generated by psc version 0.10.1
"use strict";
var Prelude = require("../Prelude");
var Data_Generic = require("../Data.Generic");
var CSS_Common = require("../CSS.Common");
var CSS_Property = require("../CSS.Property");
var CSS_Size = require("../CSS.Size");
var CSS_String = require("../CSS.String");
var CSS_Stylesheet = require("../CSS.Stylesheet");
var Data_Eq = require("../Data.Eq");
var Data_Ord = require("../Data.Ord");
var Control_Apply = require("../Control.Apply");
var Data_Maybe = require("../Data.Maybe");
var Data_Unit = require("../Data.Unit");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Function = require("../Data.Function");
var Data_Show = require("../Data.Show");
var Data_Tuple = require("../Data.Tuple");
var JustifyContentValue = function (x) {
    return x;
};
var FlexWrap = function (x) {
    return x;
};
var FlexDirection = function (x) {
    return x;
};
var AlignSelfValue = function (x) {
    return x;
};
var AlignItemsValue = function (x) {
    return x;
};
var AlignContentValue = function (x) {
    return x;
};
var FlexEnd = function (flexEnd) {
    this.flexEnd = flexEnd;
};
var FlexStart = function (flexStart) {
    this.flexStart = flexStart;
};
var SpaceAround = function (spaceAround) {
    this.spaceAround = spaceAround;
};
var SpaceBetween = function (spaceBetween) {
    this.spaceBetween = spaceBetween;
};
var Stretch = function (stretch) {
    this.stretch = stretch;
};
var wrapReverse = FlexWrap(CSS_String.fromString(CSS_Property.isStringValue)("wrap-reverse"));
var wrap = FlexWrap(CSS_String.fromString(CSS_Property.isStringValue)("wrap"));
var valJustifyContentValue = new CSS_Property.Val(function (v) {
    return v;
});
var valFlexWrap = new CSS_Property.Val(function (v) {
    return v;
});
var valFlexDirection = new CSS_Property.Val(function (v) {
    return v;
});
var valAlignSelfValue = new CSS_Property.Val(function (v) {
    return v;
});
var valAlignItemsValue = new CSS_Property.Val(function (v) {
    return v;
});
var valAlignContentValue = new CSS_Property.Val(function (v) {
    return v;
});
var stretchValue = new Stretch(CSS_String.fromString(CSS_Property.isStringValue)("stretch"));
var stretch = function (dict) {
    return dict.stretch;
};
var spaceBetweenValue = new SpaceBetween(CSS_String.fromString(CSS_Property.isStringValue)("space-between"));
var spaceBetween = function (dict) {
    return dict.spaceBetween;
};
var spaceAroundValue = new SpaceAround(CSS_String.fromString(CSS_Property.isStringValue)("space-around"));
var spaceAround = function (dict) {
    return dict.spaceAround;
};
var rowReverse = FlexDirection(CSS_String.fromString(CSS_Property.isStringValue)("row-reverse"));
var row = FlexDirection(CSS_String.fromString(CSS_Property.isStringValue)("row"));
var otherJustifyContentValue = new CSS_Common.Other(JustifyContentValue);
var otherFlexWrap = new CSS_Common.Other(FlexWrap);
var otherFlexDirection = new CSS_Common.Other(FlexDirection);
var otherAlignSelfValue = new CSS_Common.Other(AlignSelfValue);
var otherAlignItemsValue = new CSS_Common.Other(AlignItemsValue);
var otherAlignContentValue = new CSS_Common.Other(AlignContentValue);
var order = function (i) {
    return CSS_Stylesheet.key(CSS_Property.valValue)(CSS_String.fromString(CSS_Property.isStringKey)("order"))(CSS_String.fromString(CSS_Property.isStringValue)(Data_Show.show(Data_Show.showInt)(i)));
};
var nowrap = FlexWrap(CSS_String.fromString(CSS_Property.isStringValue)("nowrap"));
var justifyContent = CSS_Stylesheet.key(valJustifyContentValue)(CSS_String.fromString(CSS_Property.isStringKey)("justify-content"));
var isStringJustifyContentValue = new CSS_String.IsString(function ($138) {
    return JustifyContentValue(CSS_String.fromString(CSS_Property.isStringValue)($138));
});
var spaceAroundJustifyContentValue = new SpaceAround(CSS_String.fromString(isStringJustifyContentValue)("space-around"));
var spaceBetweenJustifyContentValue = new SpaceBetween(CSS_String.fromString(isStringJustifyContentValue)("space-between"));
var isStringAlignSelfValue = new CSS_String.IsString(function ($139) {
    return AlignSelfValue(CSS_String.fromString(CSS_Property.isStringValue)($139));
});
var stretchAlignSelfValue = new Stretch(CSS_String.fromString(isStringAlignSelfValue)("stretch"));
var isStringAlignItemsValue = new CSS_String.IsString(function ($140) {
    return AlignItemsValue(CSS_String.fromString(CSS_Property.isStringValue)($140));
});
var stretchAlignItemsValue = new Stretch(CSS_String.fromString(isStringAlignItemsValue)("stretch"));
var isStringAlignContentValue = new CSS_String.IsString(function ($141) {
    return AlignContentValue(CSS_String.fromString(CSS_Property.isStringValue)($141));
});
var spaceAroundAlignContentValue = new SpaceAround(CSS_String.fromString(isStringAlignContentValue)("space-around"));
var spaceBetweenAlignContentValue = new SpaceBetween(CSS_String.fromString(isStringAlignContentValue)("space-between"));
var stretchAlignContentValue = new Stretch(CSS_String.fromString(isStringAlignContentValue)("stretch"));
var inheritJustifyContentValue = new CSS_Common.Inherit(CSS_String.fromString(isStringJustifyContentValue)("inherit"));
var inheritAlignSelfValue = new CSS_Common.Inherit(CSS_String.fromString(isStringAlignSelfValue)("inherit"));
var inheritAlignItemsValue = new CSS_Common.Inherit(CSS_String.fromString(isStringAlignItemsValue)("inherit"));
var inheritAlignContentValue = new CSS_Common.Inherit(CSS_String.fromString(isStringAlignContentValue)("inherit"));
var genericJustifyContentValue = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Flexbox.JustifyContentValue" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(JustifyContentValue))(Data_Generic.fromSpine(CSS_Property.genericValue)(v.value1[0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Flexbox.JustifyContentValue", [ {
        sigConstructor: "CSS.Flexbox.JustifyContentValue", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Property.genericValue)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("CSS.Flexbox.JustifyContentValue", [ function ($dollarq) {
        return Data_Generic.toSpine(CSS_Property.genericValue)(v);
    } ]);
});
var genericFlexWrap = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Flexbox.FlexWrap" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(FlexWrap))(Data_Generic.fromSpine(CSS_Property.genericValue)(v.value1[0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Flexbox.FlexWrap", [ {
        sigConstructor: "CSS.Flexbox.FlexWrap", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Property.genericValue)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("CSS.Flexbox.FlexWrap", [ function ($dollarq) {
        return Data_Generic.toSpine(CSS_Property.genericValue)(v);
    } ]);
});
var genericFlexDirection = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Flexbox.FlexDirection" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(FlexDirection))(Data_Generic.fromSpine(CSS_Property.genericValue)(v.value1[0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Flexbox.FlexDirection", [ {
        sigConstructor: "CSS.Flexbox.FlexDirection", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Property.genericValue)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("CSS.Flexbox.FlexDirection", [ function ($dollarq) {
        return Data_Generic.toSpine(CSS_Property.genericValue)(v);
    } ]);
});
var genericAlignSelfValue = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Flexbox.AlignSelfValue" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(AlignSelfValue))(Data_Generic.fromSpine(CSS_Property.genericValue)(v.value1[0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Flexbox.AlignSelfValue", [ {
        sigConstructor: "CSS.Flexbox.AlignSelfValue", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Property.genericValue)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("CSS.Flexbox.AlignSelfValue", [ function ($dollarq) {
        return Data_Generic.toSpine(CSS_Property.genericValue)(v);
    } ]);
});
var genericAlignItemsValue = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Flexbox.AlignItemsValue" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(AlignItemsValue))(Data_Generic.fromSpine(CSS_Property.genericValue)(v.value1[0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Flexbox.AlignItemsValue", [ {
        sigConstructor: "CSS.Flexbox.AlignItemsValue", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Property.genericValue)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("CSS.Flexbox.AlignItemsValue", [ function ($dollarq) {
        return Data_Generic.toSpine(CSS_Property.genericValue)(v);
    } ]);
});
var genericAlignContentValue = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Flexbox.AlignContentValue" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(AlignContentValue))(Data_Generic.fromSpine(CSS_Property.genericValue)(v.value1[0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Flexbox.AlignContentValue", [ {
        sigConstructor: "CSS.Flexbox.AlignContentValue", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Property.genericValue)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("CSS.Flexbox.AlignContentValue", [ function ($dollarq) {
        return Data_Generic.toSpine(CSS_Property.genericValue)(v);
    } ]);
});
var flexWrap = CSS_Stylesheet.key(valFlexWrap)(CSS_String.fromString(CSS_Property.isStringKey)("flex-wrap"));
var flexStartValue = new FlexStart(CSS_String.fromString(CSS_Property.isStringValue)("flex-start"));
var flexStartJustifyContentValue = new FlexStart(CSS_String.fromString(isStringJustifyContentValue)("flex-start"));
var flexStartAlignSelfValue = new FlexStart(CSS_String.fromString(isStringAlignSelfValue)("flex-start"));
var flexStartAlignItemsValue = new FlexStart(CSS_String.fromString(isStringAlignItemsValue)("flex-start"));
var flexStartAlignContentValue = new FlexStart(CSS_String.fromString(isStringAlignContentValue)("flex-start"));
var flexStart = function (dict) {
    return dict.flexStart;
};
var flexShrink = function (i) {
    return CSS_Stylesheet.key(CSS_Property.valValue)(CSS_String.fromString(CSS_Property.isStringKey)("flex-shrink"))(CSS_String.fromString(CSS_Property.isStringValue)(Data_Show.show(Data_Show.showInt)(i)));
};
var flexGrow = function (i) {
    return CSS_Stylesheet.key(CSS_Property.valValue)(CSS_String.fromString(CSS_Property.isStringKey)("flex-grow"))(CSS_String.fromString(CSS_Property.isStringValue)(Data_Show.show(Data_Show.showInt)(i)));
};
var flexFlow = function (d) {
    return function (w) {
        return CSS_Stylesheet.key(CSS_Property.valTuple(valFlexDirection)(valFlexWrap))(CSS_String.fromString(CSS_Property.isStringKey)("flex-flow"))(new Data_Tuple.Tuple(d, w));
    };
};
var flexEndValue = new FlexEnd(CSS_String.fromString(CSS_Property.isStringValue)("flex-end"));
var flexEndJustifyContentValue = new FlexEnd(CSS_String.fromString(isStringJustifyContentValue)("flex-end"));
var flexEndAlignSelfValue = new FlexEnd(CSS_String.fromString(isStringAlignSelfValue)("flex-end"));
var flexEndAlignItemsValue = new FlexEnd(CSS_String.fromString(isStringAlignItemsValue)("flex-end"));
var flexEndAlignContentValue = new FlexEnd(CSS_String.fromString(isStringAlignContentValue)("flex-end"));
var flexEnd = function (dict) {
    return dict.flexEnd;
};
var flexDirection = CSS_Stylesheet.key(valFlexDirection)(CSS_String.fromString(CSS_Property.isStringKey)("flex-direction"));
var flexBasis = CSS_Stylesheet.key(CSS_Size.valSize)(CSS_String.fromString(CSS_Property.isStringKey)("flex-basis"));
var flex = function (g) {
    return function (s) {
        return function (b) {
            var ss = CSS_String.fromString(CSS_Property.isStringValue)(Data_Show.show(Data_Show.showInt)(s));
            var gs = CSS_String.fromString(CSS_Property.isStringValue)(Data_Show.show(Data_Show.showInt)(g));
            return CSS_Stylesheet.key(CSS_Property.valTuple(CSS_Property.valValue)(CSS_Property.valTuple(CSS_Property.valValue)(CSS_Property.valValue)))(CSS_String.fromString(CSS_Property.isStringKey)("flex"))(new Data_Tuple.Tuple(gs, new Data_Tuple.Tuple(ss, CSS_Property.value(CSS_Size.valSize)(b))));
        };
    };
};
var eqJustifyContentValue = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(CSS_Property.eqValue)(x)(y);
    };
});
var ordJustifyContentValue = new Data_Ord.Ord(function () {
    return eqJustifyContentValue;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(CSS_Property.ordValue)(x)(y);
    };
});
var eqFlexWrap = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(CSS_Property.eqValue)(x)(y);
    };
});
var ordFlexWrap = new Data_Ord.Ord(function () {
    return eqFlexWrap;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(CSS_Property.ordValue)(x)(y);
    };
});
var eqFlexDirection = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(CSS_Property.eqValue)(x)(y);
    };
});
var ordFlexDirection = new Data_Ord.Ord(function () {
    return eqFlexDirection;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(CSS_Property.ordValue)(x)(y);
    };
});
var eqAlignSelfValue = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(CSS_Property.eqValue)(x)(y);
    };
});
var ordAlignSelfValue = new Data_Ord.Ord(function () {
    return eqAlignSelfValue;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(CSS_Property.ordValue)(x)(y);
    };
});
var eqAlignItemsValue = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(CSS_Property.eqValue)(x)(y);
    };
});
var ordAlignItemsValue = new Data_Ord.Ord(function () {
    return eqAlignItemsValue;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(CSS_Property.ordValue)(x)(y);
    };
});
var eqAlignContentValue = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(CSS_Property.eqValue)(x)(y);
    };
});
var ordAlignContentValue = new Data_Ord.Ord(function () {
    return eqAlignContentValue;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(CSS_Property.ordValue)(x)(y);
    };
});
var columnReverse = FlexDirection(CSS_String.fromString(CSS_Property.isStringValue)("column-reverse"));
var column = FlexDirection(CSS_String.fromString(CSS_Property.isStringValue)("column"));
var centerJustifyContentValue = new CSS_Common.Center(CSS_String.fromString(isStringJustifyContentValue)("center"));
var centerAlignSelfValue = new CSS_Common.Center(CSS_String.fromString(isStringAlignSelfValue)("center"));
var centerAlignItemsValue = new CSS_Common.Center(CSS_String.fromString(isStringAlignItemsValue)("center"));
var centerAlignContentValue = new CSS_Common.Center(CSS_String.fromString(isStringAlignContentValue)("center"));
var baselineAlignSelfValue = new CSS_Common.Baseline(CSS_String.fromString(isStringAlignSelfValue)("baseline"));
var baselineAlignItemsValue = new CSS_Common.Baseline(CSS_String.fromString(isStringAlignItemsValue)("baseline"));
var autoAlignSelfValue = new CSS_Common.Auto(CSS_String.fromString(isStringAlignSelfValue)("auto"));
var alignSelf = CSS_Stylesheet.key(valAlignSelfValue)(CSS_String.fromString(CSS_Property.isStringKey)("align-self"));
var alignItems = CSS_Stylesheet.key(valAlignItemsValue)(CSS_String.fromString(CSS_Property.isStringKey)("align-items"));
var alignContent = CSS_Stylesheet.key(valAlignContentValue)(CSS_String.fromString(CSS_Property.isStringKey)("align-content"));
module.exports = {
    AlignContentValue: AlignContentValue, 
    AlignItemsValue: AlignItemsValue, 
    AlignSelfValue: AlignSelfValue, 
    FlexDirection: FlexDirection, 
    FlexWrap: FlexWrap, 
    JustifyContentValue: JustifyContentValue, 
    FlexEnd: FlexEnd, 
    FlexStart: FlexStart, 
    SpaceAround: SpaceAround, 
    SpaceBetween: SpaceBetween, 
    Stretch: Stretch, 
    alignContent: alignContent, 
    alignItems: alignItems, 
    alignSelf: alignSelf, 
    column: column, 
    columnReverse: columnReverse, 
    flex: flex, 
    flexBasis: flexBasis, 
    flexDirection: flexDirection, 
    flexEnd: flexEnd, 
    flexFlow: flexFlow, 
    flexGrow: flexGrow, 
    flexShrink: flexShrink, 
    flexStart: flexStart, 
    flexWrap: flexWrap, 
    justifyContent: justifyContent, 
    nowrap: nowrap, 
    order: order, 
    row: row, 
    rowReverse: rowReverse, 
    spaceAround: spaceAround, 
    spaceBetween: spaceBetween, 
    stretch: stretch, 
    wrap: wrap, 
    wrapReverse: wrapReverse, 
    flexEndValue: flexEndValue, 
    flexStartValue: flexStartValue, 
    spaceAroundValue: spaceAroundValue, 
    spaceBetweenValue: spaceBetweenValue, 
    stretchValue: stretchValue, 
    eqAlignContentValue: eqAlignContentValue, 
    ordAlignContentValue: ordAlignContentValue, 
    genericAlignContentValue: genericAlignContentValue, 
    isStringAlignContentValue: isStringAlignContentValue, 
    valAlignContentValue: valAlignContentValue, 
    otherAlignContentValue: otherAlignContentValue, 
    inheritAlignContentValue: inheritAlignContentValue, 
    flexStartAlignContentValue: flexStartAlignContentValue, 
    flexEndAlignContentValue: flexEndAlignContentValue, 
    centerAlignContentValue: centerAlignContentValue, 
    spaceBetweenAlignContentValue: spaceBetweenAlignContentValue, 
    spaceAroundAlignContentValue: spaceAroundAlignContentValue, 
    stretchAlignContentValue: stretchAlignContentValue, 
    eqAlignItemsValue: eqAlignItemsValue, 
    ordAlignItemsValue: ordAlignItemsValue, 
    genericAlignItemsValue: genericAlignItemsValue, 
    isStringAlignItemsValue: isStringAlignItemsValue, 
    valAlignItemsValue: valAlignItemsValue, 
    otherAlignItemsValue: otherAlignItemsValue, 
    inheritAlignItemsValue: inheritAlignItemsValue, 
    baselineAlignItemsValue: baselineAlignItemsValue, 
    centerAlignItemsValue: centerAlignItemsValue, 
    flexEndAlignItemsValue: flexEndAlignItemsValue, 
    flexStartAlignItemsValue: flexStartAlignItemsValue, 
    stretchAlignItemsValue: stretchAlignItemsValue, 
    eqAlignSelfValue: eqAlignSelfValue, 
    ordAlignSelfValue: ordAlignSelfValue, 
    genericAlignSelfValue: genericAlignSelfValue, 
    isStringAlignSelfValue: isStringAlignSelfValue, 
    valAlignSelfValue: valAlignSelfValue, 
    otherAlignSelfValue: otherAlignSelfValue, 
    inheritAlignSelfValue: inheritAlignSelfValue, 
    autoAlignSelfValue: autoAlignSelfValue, 
    baselineAlignSelfValue: baselineAlignSelfValue, 
    centerAlignSelfValue: centerAlignSelfValue, 
    flexEndAlignSelfValue: flexEndAlignSelfValue, 
    flexStartAlignSelfValue: flexStartAlignSelfValue, 
    stretchAlignSelfValue: stretchAlignSelfValue, 
    eqFlexDirection: eqFlexDirection, 
    ordFlexDirection: ordFlexDirection, 
    genericFlexDirection: genericFlexDirection, 
    valFlexDirection: valFlexDirection, 
    otherFlexDirection: otherFlexDirection, 
    eqFlexWrap: eqFlexWrap, 
    ordFlexWrap: ordFlexWrap, 
    genericFlexWrap: genericFlexWrap, 
    valFlexWrap: valFlexWrap, 
    otherFlexWrap: otherFlexWrap, 
    eqJustifyContentValue: eqJustifyContentValue, 
    ordJustifyContentValue: ordJustifyContentValue, 
    genericJustifyContentValue: genericJustifyContentValue, 
    isStringJustifyContentValue: isStringJustifyContentValue, 
    valJustifyContentValue: valJustifyContentValue, 
    otherJustifyContentValue: otherJustifyContentValue, 
    inheritJustifyContentValue: inheritJustifyContentValue, 
    centerJustifyContentValue: centerJustifyContentValue, 
    flexEndJustifyContentValue: flexEndJustifyContentValue, 
    flexStartJustifyContentValue: flexStartJustifyContentValue, 
    spaceAroundJustifyContentValue: spaceAroundJustifyContentValue, 
    spaceBetweenJustifyContentValue: spaceBetweenJustifyContentValue
};