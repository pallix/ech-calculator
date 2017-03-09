import purs from "rollup-plugin-purs";
import sourcemaps from "rollup-plugin-sourcemaps";

export default {
  entry: "src/Rainwater.purs",
  dest: "bundle.js",
  format: "iife",
  sourceMap: true,
  plugins: [
    purs()
  ]
};
