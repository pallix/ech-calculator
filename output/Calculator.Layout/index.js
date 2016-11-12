"use strict";
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var CSS = require("../CSS");
var CSS_Render = require("../CSS.Render");
var Control_Bind = require("../Control.Bind");
var CSS_Stylesheet = require("../CSS.Stylesheet");
var CSS_Elements = require("../CSS.Elements");
var CSS_Font = require("../CSS.Font");
var Color_Scheme_X11 = require("../Color.Scheme.X11");
var CSS_String = require("../CSS.String");
var CSS_Selector = require("../CSS.Selector");
var CSS_Display = require("../CSS.Display");
var toCss = function (r) {
    var $0 = CSS_Render.renderedSheet(r);
    if ($0 instanceof Data_Maybe.Nothing) {
        return "";
    };
    if ($0 instanceof Data_Maybe.Just) {
        return $0.value0;
    };
    throw new Error("Failed pattern match at Calculator.Layout line 17, column 11 - line 19, column 26: " + [ $0.constructor.name ]);
};
var example4 = CSS_Render.render(Control_Bind.bind(CSS_Stylesheet.bindStyleM)(CSS_Stylesheet.select(CSS_Elements.body)(CSS_Font.color(Color_Scheme_X11.blue)))(function () {
    return CSS_Stylesheet.select(CSS_String.fromString(CSS_Selector.isStringSelector)("#world"))(CSS_Display.display(CSS_Display.block));
}));
var layout = toCss(example4);
module.exports = {
    layout: layout
};
