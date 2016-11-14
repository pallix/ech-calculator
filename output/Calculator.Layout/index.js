"use strict";
var Calculator_Model = require("../Calculator.Model");
var CSS = require("../CSS");
var DOM_Node_Types = require("../DOM.Node.Types");
var Data_Array = require("../Data.Array");
var Data_Foldable = require("../Data.Foldable");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Text_Smolder_HTML = require("../Text.Smolder.HTML");
var Text_Smolder_HTML_Attributes = require("../Text.Smolder.HTML.Attributes");
var Text_Smolder_Markup = require("../Text.Smolder.Markup");
var Prelude = require("../Prelude");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var CSS_Render = require("../CSS.Render");
var Data_Function = require("../Data.Function");
var Control_Bind = require("../Control.Bind");
var Data_Show = require("../Data.Show");
var CSS_Stylesheet = require("../CSS.Stylesheet");
var CSS_Elements = require("../CSS.Elements");
var CSS_Font = require("../CSS.Font");
var Color_Scheme_X11 = require("../Color.Scheme.X11");
var CSS_String = require("../CSS.String");
var CSS_Selector = require("../CSS.Selector");
var CSS_Display = require("../CSS.Display");
var Data_Semigroup = require("../Data.Semigroup");

/**
 *  controls ::
 */
var tokenList = function ($39) {
    return Text_Smolder_HTML.ul(Data_Foldable.foldMap(Data_Foldable.foldableArray)(Text_Smolder_Markup.monoidMarkup)(function ($40) {
        return Text_Smolder_HTML.li(Text_Smolder_Markup.text((function (v) {
            return v.title;
        })($40)));
    })($39));
};
var toCss = function (r) {
    var $8 = CSS_Render.renderedSheet(r);
    if ($8 instanceof Data_Maybe.Nothing) {
        return "";
    };
    if ($8 instanceof Data_Maybe.Just) {
        return $8.value0;
    };
    throw new Error("Failed pattern match at Calculator.Layout line 25, column 11 - line 27, column 26: " + [ $8.constructor.name ]);
};

/**
 *  light :: forall e. Boolean -> Markup e
 *  light on = div ! arg $ mempty
 *    where arg | on = className "on"
 *              | otherwise = mempty
 */
var hex = function (hover) {
    return function (grid) {
        return function (item) {
            var image = function (v) {
                if (v === "Food") {
                    return "https://farm5.staticflickr.com/4144/5053682635_b348b24698.jpg";
                };
                if (v === "Bin") {
                    return "https://farm5.staticflickr.com/4144/5053682635_b348b24698.jpg";
                };
                if (v === "Compost") {
                    return "https://farm5.staticflickr.com/4144/5053682635_b348b24698.jpg";
                };
                if (v === "Garden") {
                    return "https://farm5.staticflickr.com/4144/5053682635_b348b24698.jpg";
                };
                if (v === "Food Garden") {
                    return "https://farm5.staticflickr.com/4144/5053682635_b348b24698.jpg";
                };
                if (v === "Shopped Food") {
                    return "https://farm5.staticflickr.com/4144/5053682635_b348b24698.jpg";
                };
                if (v === "_") {
                    return "https://dummyimage.com/200x200&text=+";
                };
                return "";
            };
            var hoverClass = function (v) {
                return function (v1) {
                    if (!v && !v1) {
                        return Text_Smolder_HTML_Attributes.className("hexIn");
                    };
                    if (v && !v1) {
                        return Text_Smolder_HTML_Attributes.className("hexIn hover");
                    };
                    if (v1) {
                        return Text_Smolder_HTML_Attributes.className("hexIn grid");
                    };
                    throw new Error("Failed pattern match at Calculator.Layout line 41, column 1 - line 63, column 1: " + [ v.constructor.name, v1.constructor.name ]);
                };
            };
            var tokenToHex = function (v) {
                if (v.title === "") {
                    return Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupMF)(Text_Smolder_HTML.a)(hoverClass(hover)(grid))(Data_Monoid.mempty(Text_Smolder_Markup.monoidMarkup));
                };
                return Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupMF)(Text_Smolder_HTML.a)(hoverClass(hover)(grid))(Control_Bind.bind(Text_Smolder_Markup.bindMarkupM)(Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupM)(Text_Smolder_HTML.img)(Text_Smolder_HTML_Attributes.src(image(item.title))))(function () {
                    return Control_Bind.bind(Text_Smolder_Markup.bindMarkupM)(Text_Smolder_HTML.h2(Text_Smolder_Markup.text(v.title)))(function () {
                        return Text_Smolder_HTML.p(Text_Smolder_Markup.text("Details"));
                    });
                }));
            };
            return Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupMF)(Text_Smolder_HTML.li)(Text_Smolder_HTML_Attributes.className("hex"))(tokenToHex(item));
        };
    };
};
var flow = function (item) {
    var tokenToHex = function (v) {
        if (v.title === "_") {
            return Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupMF)(Text_Smolder_HTML.li)(Text_Smolder_HTML_Attributes.className("hex"))(Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupMF)(Text_Smolder_HTML.a)(Text_Smolder_HTML_Attributes.className("hexIn hover"))(Control_Bind.bind(Text_Smolder_Markup.bindMarkupM)(Text_Smolder_HTML.h2(Text_Smolder_Markup.text(Data_Show.show(Data_Show.showNumber)(v.quantity))))(function () {
                return Control_Bind.bind(Text_Smolder_Markup.bindMarkupM)(Text_Smolder_HTML.p(Text_Smolder_Markup.text("")))(function () {
                    return Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupMF)(Text_Smolder_HTML.div)(Text_Smolder_HTML_Attributes.className("arrow-css right"))(Data_Monoid.mempty(Text_Smolder_Markup.monoidMarkup));
                });
            })));
        };
        if (v.title === "/") {
            return Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupMF)(Text_Smolder_HTML.li)(Text_Smolder_HTML_Attributes.className("hex rotate-1"))(Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupMF)(Text_Smolder_HTML.a)(Text_Smolder_HTML_Attributes.className("hexIn hover"))(Control_Bind.bind(Text_Smolder_Markup.bindMarkupM)(Text_Smolder_HTML.h2(Text_Smolder_Markup.text(Data_Show.show(Data_Show.showNumber)(v.quantity))))(function () {
                return Control_Bind.bind(Text_Smolder_Markup.bindMarkupM)(Text_Smolder_HTML.p(Text_Smolder_Markup.text("")))(function () {
                    return Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupMF)(Text_Smolder_HTML.div)(Text_Smolder_HTML_Attributes.className("arrow-css right"))(Data_Monoid.mempty(Text_Smolder_Markup.monoidMarkup));
                });
            })));
        };
        return Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupMF)(Text_Smolder_HTML.li)(Text_Smolder_HTML_Attributes.className("hex"))(Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupMF)(Text_Smolder_HTML.a)(Text_Smolder_HTML_Attributes.className("hexIn hover"))(Data_Monoid.mempty(Text_Smolder_Markup.monoidMarkup)));
    };
    return tokenToHex(item);
};
var example4 = CSS_Render.render(Control_Bind.bind(CSS_Stylesheet.bindStyleM)(CSS_Stylesheet.select(CSS_Elements.body)(CSS_Font.color(Color_Scheme_X11.blue)))(function () {
    return CSS_Stylesheet.select(CSS_String.fromString(CSS_Selector.isStringSelector)("#world"))(CSS_Display.display(CSS_Display.block));
}));
var layout = toCss(example4);
var emptyArrow = {
    title: "", 
    quantity: 0.0
};
var css = Text_Smolder_HTML.style(Text_Smolder_Markup.text(layout));

/**
 *  $ do
 *   img ! src "https://dummyimage.com/200x200&text=+"
 */
var arrayHex = function (v) {
    if (v.length === 1) {
        return Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(9)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(4)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.singleton(v[0]))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(4)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)({
            title: ""
        }))(Data_Array.replicate(9)({
            title: ""
        }))))))));
    };
    if (v.length === 2) {
        return Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(9)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(3)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.singleton(v[0]))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(1)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.singleton(v[1]))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(3)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)({
            title: ""
        }))(Data_Array.replicate(9)({
            title: ""
        }))))))))));
    };
    if (v.length === 3) {
        return Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(4)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.singleton(v[2]))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(4)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(3)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.singleton(v[0]))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(1)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.singleton(v[1]))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(3)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)({
            title: ""
        }))(Data_Array.replicate(9)({
            title: ""
        }))))))))))));
    };
    if (v.length === 4) {
        return Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(4)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.singleton(v[2]))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(4)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(3)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.singleton(v[0]))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(1)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.singleton(v[1]))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(3)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(4)({
            title: ""
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.singleton(v[3]))(Data_Array.replicate(4)({
            title: ""
        }))))))))))))));
    };
    return Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)({
        title: ""
    }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(9)({
        title: ""
    }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)({
        title: ""
    }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(9)({
        title: ""
    }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)({
        title: ""
    }))(Data_Array.replicate(9)({
        title: ""
    }))))));
};
var hexes = function (hover) {
    return function (grid) {
        return function (arr) {
            return Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupMF)(Text_Smolder_HTML.ul)(Text_Smolder_HTML_Attributes.className("hexGrid"))(Data_Foldable.foldMap(Data_Foldable.foldableArray)(Text_Smolder_Markup.monoidMarkup)(hex(hover)(grid))(arrayHex(arr)));
        };
    };
};
var arrayArrow = function (v) {
    if (v.length === 1) {
        return Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(9)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(3)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.singleton({
            title: "_", 
            quantity: 2.0
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.singleton(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.singleton({
            title: "_", 
            quantity: 2.0
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(3)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)(emptyArrow))(Data_Array.replicate(9)(emptyArrow))))))))));
    };
    if (v.length === 2) {
        return Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(9)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(4)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.singleton({
            title: "_", 
            quantity: 2.0
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(4)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)(emptyArrow))(Data_Array.replicate(9)(emptyArrow))))))));
    };
    if (v.length === 3) {
        return Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(9)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(4)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.singleton({
            title: "/", 
            quantity: 2.0
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(5)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(4)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.singleton({
            title: "_", 
            quantity: 2.0
        }))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(4)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)(emptyArrow))(Data_Array.replicate(9)(emptyArrow))))))))));
    };
    return Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(9)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(9)(emptyArrow))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)(emptyArrow))(Data_Array.replicate(9)(emptyArrow))))));
};
var arrows = function (hover) {
    return function (grid) {
        return function (arr) {
            return Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupMF)(Text_Smolder_HTML.ul)(Text_Smolder_HTML_Attributes.className("hexGrid flows"))(Data_Foldable.foldMap(Data_Foldable.foldableArray)(Text_Smolder_Markup.monoidMarkup)(flow)(arrayArrow(arr)));
        };
    };
};
var $$interface = function (hover) {
    return function (grid) {
        return function (arr) {
            return Control_Bind.bind(Text_Smolder_Markup.bindMarkupM)(arrows(true)(false)(arr))(function () {
                return hexes(hover)(grid)(arr);
            });
        };
    };
};
module.exports = {
    "interface": $$interface
};