"use strict";
var Prelude = require("../Prelude");
var Calculator_Model = require("../Calculator.Model");
var Calculator_Layout = require("../Calculator.Layout");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Console = require("../Control.Monad.Eff.Console");
var $$Math = require("../Math");
var Data_Int = require("../Data.Int");
var Data_Array = require("../Data.Array");
var Data_Monoid = require("../Data.Monoid");
var Data_Foldable = require("../Data.Foldable");
var DOM = require("../DOM");
var Signal_Channel = require("../Signal.Channel");
var Graphics_Canvas = require("../Graphics.Canvas");
var Control_Monad_Eff_Timer = require("../Control.Monad.Eff.Timer");
var Graphics_Drawing = require("../Graphics.Drawing");
var Graphics_Drawing_Font = require("../Graphics.Drawing.Font");
var Text_Smolder_HTML = require("../Text.Smolder.HTML");
var Text_Smolder_Markup = require("../Text.Smolder.Markup");
var Text_Smolder_HTML_Attributes = require("../Text.Smolder.HTML.Attributes");
var Signal_DOM = require("../Signal.DOM");
var Signal_Time = require("../Signal.Time");
var Flare = require("../Flare");
var Flare_Smolder = require("../Flare.Smolder");
var Data_Boolean = require("../Data.Boolean");
var Data_Function = require("../Data.Function");
var Control_Bind = require("../Control.Bind");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Functor = require("../Data.Functor");
var Control_Applicative = require("../Control.Applicative");

/**
 * 
 *  center :: Point
 *  center = { x : 400.0, y: 300.0 }
 * 
 *  delta :: Point
 *  delta = { x : 200.0, y: 0.0 }
 * 
 *  blue :: Color
 *  blue = rgb 0 0 255
 * 
 *  polygon :: Int -> Color -> Drawing
 *  polygon sides col =
 *    filled (fillColor col) $
 *      closed $ poly sides
 * 
 *    where poly n = do
 *                      i <- 0..n
 *                      let theta = pi / ( toNumber n / 2.0 ) * toNumber i
 *                      pure { x: 50.0 * sin theta, y: 50.0 * cos theta }
 * 
 *  hexagon :: Color -> Drawing
 *  hexagon col = polygon 6 col
 * 
 *  token :: String -> Color -> Drawing
 *  token t col = hexagon col <> text ( font sansSerif 16 bold ) 0.0 0.0 (fillColor white) t
 * 
 *  arrow :: Number -> Number -> Drawing
 *  arrow v t = outlined (outlineColor (rgba 0 0 255 opacity) <> (lineWidth v )) (
 *                path [ { x: arrowBegin.x , y: arrowBegin.y },
 *                       { x: arrowEnd.x   , y: arrowEnd.y } ])
 *             <> outlined (outlineColor (rgba 0 0 255 opacity) <> (lineWidth v )) (
 *                  path [ { x: arrowEnd.x - arrowSize  , y: arrowEnd.y - arrowSize },
 *                         { x: arrowEnd.x   , y: arrowEnd.y } ])
 *             <> outlined (outlineColor (rgba 0 0 255 opacity) <> (lineWidth v )) (
 *                  path [ { x: arrowEnd.x - arrowSize , y: arrowEnd.y + arrowSize },
 *                         { x: arrowEnd.x , y: arrowEnd.y } ])
 *                where
 *                  opacity = ( cos ( t / 300.0 ) / 2.0 ) + 1.0
 *                  arrowBegin = { x: (center.x - delta.x + ( delta.x / 4.0 ) ), y: ( center.y - delta.y + ( delta.y / 4.0 ) ) }
 *                  arrowEnd = { x: (center.x + delta.x - ( delta.x / 4.0 ) ), y: ( center.y + delta.y - ( delta.y / 4.0 ) ) }
 *                  arrowSize = 20.0
 * 
 * 
 *  foodToBin :: Number -> Number -> Drawing
 *  foodToBin v t = ( translateLeft ( token "Food" blue ) )
 *               <> ( translateRight ( token "Bin" blue ) )
 *               <> ( arrow v t )
 *               <> ( text ( font sansSerif 16 bold ) center.x ( center.y - 10.0 )  (fillColor blue) (show v) )
 *                where
 *                  translateRight = translate ( center.x + delta.x ) ( center.y + delta.y )
 *                  translateLeft = translate ( center.x - delta.x ) ( center.y - delta.y )
 * 
 *  uidrawing :: forall e. UI (timer :: TIMER | e) Drawing
 *  uidrawing = foodToBin <$> (numberSlider "value"  0.0 10.0 1.0  7.0)
 *                 <*> lift animationFrame
 * 
 */
var light = function (on) {
    var arg = (function () {
        if (on) {
            return Text_Smolder_HTML_Attributes.className("on");
        };
        if (Data_Boolean.otherwise) {
            return Data_Monoid.mempty(Text_Smolder_Markup.monoidAttribute);
        };
        throw new Error("Failed pattern match at Main line 98, column 9 - line 99, column 33: " + [  ]);
    })();
    return Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupMF)(Text_Smolder_HTML.div)(arg)(Data_Monoid.mempty(Text_Smolder_Markup.monoidMarkup));
};
var hex = function (on) {
    return function (item) {
        var image = function (v) {
            if (v === "Food") {
                return "https://farm5.staticflickr.com/4144/5053682635_b348b24698.jpg";
            };
            if (v === "Bin") {
                return "https://farm5.staticflickr.com/4144/5053682635_b348b24698.jpg";
            };
            return "";
        };
        var hoverClass = function (v) {
            if (v) {
                return Text_Smolder_HTML_Attributes.className("hexIn");
            };
            if (!v) {
                return Text_Smolder_HTML_Attributes.className("hexIn hover");
            };
            throw new Error("Failed pattern match at Main line 102, column 1 - line 116, column 1: " + [ v.constructor.name ]);
        };
        var token = function (v) {
            if (v === "") {
                return Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupMF)(Text_Smolder_HTML.a)(hoverClass(on))(Data_Monoid.mempty(Text_Smolder_Markup.monoidMarkup));
            };
            return Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupMF)(Text_Smolder_HTML.a)(hoverClass(on))(Control_Bind.bind(Text_Smolder_Markup.bindMarkupM)(Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupM)(Text_Smolder_HTML.img)(Text_Smolder_HTML_Attributes.src(image(item))))(function () {
                return Control_Bind.bind(Text_Smolder_Markup.bindMarkupM)(Text_Smolder_HTML.h1(Text_Smolder_Markup.text(item)))(function () {
                    return Text_Smolder_HTML.p(Text_Smolder_Markup.text("Some sample text on the hexagon"));
                });
            }));
        };
        return Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupMF)(Text_Smolder_HTML.li)(Text_Smolder_HTML_Attributes.className("hex"))(token(item));
    };
};
var arrayBinToFood = Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)(""))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(9)(""))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)(""))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(2)(""))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.singleton("Food"))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(3)(""))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.singleton("Bin"))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(2)(""))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.replicate(10)(""))(Data_Array.replicate(9)(""))))))))));
var hexes = function (on) {
    return Text_Smolder_Markup["with"](Text_Smolder_Markup.attributableMarkupMF)(Text_Smolder_HTML.ul)(Text_Smolder_HTML_Attributes.className("hexGrid"))(Data_Foldable.foldMap(Data_Foldable.foldableArray)(Text_Smolder_Markup.monoidMarkup)(hex(on))(arrayBinToFood));
};

/**
 *  test :: forall a e. Markup (a -> Eff (console :: CONSOLE | e) Unit)
 *  test = with div (className "on") $ do
 *                h1 #! on "click" (\_ -> log "click") $ text "OMG HAI LOL"
 *                p $ text "This is clearly the best HTML DSL ever invented.<script>alert(\"lol pwned\");</script>"
 */
var ui = Data_Functor.map(Flare.functorUI)(Data_Semigroup.append(Data_Semigroup.semigroupFn(Text_Smolder_Markup.semigroupMarkupM))(Control_Applicative.pure(Control_Applicative.applicativeFn)(Text_Smolder_HTML.style(Text_Smolder_Markup.text(Calculator_Layout.layout))))(hexes))(Flare.boolean_(false));

/**
 *  <> light <$> liftSF (since 1000.0) (button "Switch on" unit unit)
 *  ui = token <$> string_ "Yo"
 *             <*> (color "Color" (hsl 333.0 0.6 0.5))
 */
var main = Flare_Smolder.runFlareHTML("controls")("output")(ui);
module.exports = {
    arrayBinToFood: arrayBinToFood, 
    hex: hex, 
    hexes: hexes, 
    light: light, 
    main: main, 
    ui: ui
};
