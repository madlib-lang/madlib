// file: /opt/hostedtoolcache/node/14.20.1/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Applicative.mad
import {} from "./../__internals__.mjs"
import {  } from "./Functor.mjs";
import { always } from "./Function.mjs";

window.Applicative = {};
export let apL = (Functor_b27) => (Applicative_b27) => (a => b => Applicative_b27.ap()(Functor_b27.map()(always)(a))(b));
export let liftA2 = (Functor_c54) => (Applicative_c54) => (f => x1 => x2 => (_P_ => (__$PH1__ => Applicative_c54.ap()(__$PH1__)(x2))(Functor_c54.map()(f)(_P_)))(x1));
export let liftA3 = (Functor_l89) => (Applicative_l89) => (f => x1 => x2 => x3 => (_P_ => (__$PH3__ => Applicative_l89.ap()(__$PH3__)(x3))((__$PH2__ => Applicative_l89.ap()(__$PH2__)(x2))(Functor_l89.map()(f)(_P_))))(x1));
export default { apL, liftA2, liftA3 };
