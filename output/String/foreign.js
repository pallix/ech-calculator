"use strict";

exports.append = function(x) {
    return function(y) {
        return x + y;
    };
};
