// file: /opt/hostedtoolcache/node/14.21.1/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Monad.mad
import {} from "./../__internals__.mjs"
import {  } from "./Applicative.mjs";

window.Monad = {};
export let chain2 = (Functor_a26) => (Applicative_a26) => (Monad_a26) => (f => ma => mb => Monad_a26.chain()((a => Monad_a26.chain()((b => f(a)(b)))(mb)))(ma));
export let andDo = (Functor_q42) => (Applicative_q42) => (Monad_q42) => (b => a => Monad_q42.chain()((_ => b))(a));
export default { chain2, andDo };
