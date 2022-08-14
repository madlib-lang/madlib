// file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/ByteArray.mad
import {} from "./../__internals__.mjs"
export let toString = (byteArray =>  new TextDecoder().decode(byteArray) );
export let fromString = (str =>  new TextEncoder().encode(str) );
export let toList = (byteArray =>  {
  if (byteArray.length === 0) {
    return null
  }

  let current = {}
  let result = current
  byteArray.forEach((byte) => {
    current = current.n = { v: byte, n: null }
  })
  return result.n
} );
export let fromList = (bytes =>  {
  let bytesArray = []
  while (bytes !== null) {
    bytesArray.push(bytes.v)
    bytes = bytes.n
  }
  return Uint8Array.from(bytesArray)
} );
export let concat = (byteArray1 => byteArray2 =>  {
  const result = new Uint8Array(byteArray1.length + byteArray2.length);
  result.set(byteArray1);
  result.set(byteArray2, byteArray1.length);
  return result;
} );
export let mapBytes = (f => byteArray =>  byteArray.map(f) );
export let reduce = (f => initialValue => byteArray =>  byteArray.reduce((a, b) => f(a)(b), initialValue) );
export let _$_length_$_ = (byteArray =>  byteArray.length );
export let empty = fromList((null));
export default { toString, fromString, toList, fromList, concat, mapBytes, reduce, _$_length_$_, empty };
