// file: /opt/hostedtoolcache/node/14.21.1/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Compare.mad
import {} from "./../__internals__.mjs"
window.Comparable = {};
export let MORE = 1;
export let LESS = -1;
export let EQUAL = 0;
export let eq = (Comparable_w22) => (a => b => __eq__(Comparable_w22.compare()(a)(b), EQUAL));
export let notEq = (Comparable_h33) => (a => b => !__eq__(Comparable_h33.compare()(a)(b), EQUAL));
export let gt = (Comparable_s44) => (a => b => __eq__(Comparable_s44.compare()(a)(b), MORE));
export let ge = (Comparable_d55) => (a => b => Comparable_d55.compare()(a)(b) >= EQUAL);
export let lt = (Comparable_o66) => (a => b => __eq__(Comparable_o66.compare()(a)(b), LESS));
export let le = (Comparable_z77) => (a => b => Comparable_z77.compare()(a)(b) <= EQUAL);
export default { MORE, LESS, EQUAL, eq, notEq, gt, ge, lt, le };
