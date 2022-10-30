// file: /opt/hostedtoolcache/node/14.20.1/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Url.mad
import {} from "./../__internals__.mjs"
import { Just, Nothing } from "./Maybe.mjs";

export let encode = (url =>  {
  try {
    return Just(encodeURIComponent(url))
  } catch(e) {
    return Nothing
  }
} );
export let decode = (url =>  {
    try {
      return Just(decodeURIComponent(url))
    } catch(e) {
      return Nothing
    }
  } );
export default { encode, decode };
