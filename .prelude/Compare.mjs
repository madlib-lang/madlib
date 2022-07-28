// file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/bin/prelude/__internal__/Compare.mad
import {} from "./../__internals__.mjs"
window.Comparable = {};
export let MORE = 1;
export let LESS = -1;
export let EQUAL = 0;
export let eq = (Comparable_w22) => (a => b => __eq__(Comparable_w22.compare()(a)(b), EQUAL));
export let notEq = (Comparable_g32) => (a => b => !__eq__(Comparable_g32.compare()(a)(b), EQUAL));
export let gt = (Comparable_q42) => (a => b => __eq__(Comparable_q42.compare()(a)(b), MORE));
export let ge = (Comparable_a52) => (a => b => Comparable_a52.compare()(a)(b) >= EQUAL);
export let lt = (Comparable_k62) => (a => b => __eq__(Comparable_k62.compare()(a)(b), LESS));
export let le = (Comparable_u72) => (a => b => Comparable_u72.compare()(a)(b) <= EQUAL);
export default { MORE, LESS, EQUAL, eq, notEq, gt, ge, lt, le };
