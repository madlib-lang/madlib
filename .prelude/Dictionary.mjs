// file: /opt/hostedtoolcache/node/14.21.1/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Dictionary.mad
import {} from "./../__internals__.mjs"
import {  } from "./Maybe.mjs";
import { eq, gt } from "./Compare.mjs";
import List from "./List.mjs";
import Tuple from "./Tuple.mjs";

Functor['Dictionary_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Functor['Dictionary_5b7ebeeaa5acfe1eeea5a9e9845b152d']['map'] = () => (fn => __x__ => ((__x__) => {
  if (__x__.__constructor === "Dictionary" && true) {
    let items = __x__.__args[0];
    return (_P_ => Dictionary(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((i => ([Tuple.fst(i), fn(Tuple.snd(i))])))(_P_)))(items);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
let Dictionary = (items =>  ({
  __constructor: "Dictionary",
  __args: [items],
}) );
let removeConsecutiveDoubles = (list => {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$list = list;

    while($_continue_) {
      let $list = $$list;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ !== null && __x__.v.length === 2 && true && true && __x__.n !== null && __x__.n.v.length === 2 && true && true && true) {
    let { v: [k1, v1], n: { v: [k2, v2], n: xs } } = __x__;
    (__eq__(k1, k2) ? ($_end_ = $_end_.n = { v: ([k2, v2]) }, $$list = xs, $_continue_ = true) : ($_end_ = $_end_.n = { v: ([k1, v1]) }, $$list = ({ v: ([k2, v2]), n: xs }), $_continue_ = true));
  }
  else if (__x__ !== null && true && __x__.n === null) {
    let { v: last } = __x__;
    ($_end_.n = ({ v: last, n: null }), $_result_ = $_start_.n);
  }
  else if (__x__ === null) {
    ($_end_.n = (null), $_result_ = $_start_.n);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})($list)
    }
    return $_result_;
});
export let fromList = (Comparable_m90) => (_P_ => Dictionary(removeConsecutiveDoubles(List.sortBy((a => b => Comparable_m90.compare()(Tuple.fst(a))(Tuple.fst(b))))(_P_))));
export let empty = Dictionary((null));
export let insert = (Comparable_w152) => (key => value => dict => {
    let helper = (list => {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$list = list;

    while($_continue_) {
      let $list = $$list;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ === null) {
    ($_end_.n = ({ v: ([key, value]), n: null }), $_result_ = $_start_.n);
  }
  else if (__x__ !== null && __x__.v.length === 2 && true && true && true) {
    let { v: [k, v], n: xs } = __x__;
    (eq(Comparable_w152)(k)(key) ? ($_end_.n = ({ v: ([key, value]), n: xs }), $_result_ = $_start_.n) : (gt(Comparable_w152)(k)(key) ? ($_end_.n = ({ v: ([key, value]), n: { v: ([k, v]), n: xs } }), $_result_ = $_start_.n) : ($_end_ = $_end_.n = { v: ([k, v]) }, $$list = xs, $_continue_ = true)));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})($list)
    }
    return $_result_;
});
    return ((__x__) => {
  if (__x__.__constructor === "Dictionary" && true) {
    let items = __x__.__args[0];
    return Dictionary(helper(items));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(dict);
});
export let update = (Comparable_d211) => (fn => key => dict => {
    let helper = (list => {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$list = list;

    while($_continue_) {
      let $list = $$list;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ === null) {
    ($_end_.n = (null), $_result_ = $_start_.n);
  }
  else if (__x__ !== null && __x__.v.length === 2 && true && true && true) {
    let { v: [k, v], n: xs } = __x__;
    (eq(Comparable_d211)(k)(key) ? ($_end_.n = ({ v: ([key, fn(v)]), n: xs }), $_result_ = $_start_.n) : (gt(Comparable_d211)(k)(key) ? ($_end_.n = ({ v: ([k, v]), n: xs }), $_result_ = $_start_.n) : ($_end_ = $_end_.n = { v: ([k, v]) }, $$list = xs, $_continue_ = true)));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})($list)
    }
    return $_result_;
});
    return ((__x__) => {
  if (__x__.__constructor === "Dictionary" && true) {
    let items = __x__.__args[0];
    return Dictionary(helper(items));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(dict);
});
export let get = (Comparable_s226) => (k => __x__ => ((__x__) => {
  if (__x__.__constructor === "Dictionary" && true) {
    let items = __x__.__args[0];
    return (_P_ => Functor.Maybe_b0e37fa389e368279491caac0c654a84.map()(Tuple.snd)(List.find((item => ((__x__) => {
  if (__x__.length === 2 && true && true) {
    let [kk,] = __x__;
    return __eq__(k, kk);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(item)))(_P_)))(items);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
export let merge = (Comparable_c262) => (a => b => ((__x__) => {
  if (__x__.length === 2 && __x__[0].__constructor === "Dictionary" && true && __x__[1].__constructor === "Dictionary" && true) {
    let [{ __args: [itemsA]},{ __args: [itemsB]}] = __x__;
    return fromList(Comparable_c262)(List.concat(itemsA)(itemsB));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([a, b])));
export let _$_length_$_ = (dictionary => ((__x__) => {
  if (__x__.__constructor === "Dictionary" && true) {
    let items = __x__.__args[0];
    return List._$_length_$_(items);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(dictionary));
export let toList = (dict => ((__x__) => {
  if (__x__.__constructor === "Dictionary" && true) {
    let items = __x__.__args[0];
    return items;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(dict));
export let mapM = (Functor_p327) => (Applicative_p327) => (f => dict => ((__x__) => {
  if (__x__.__constructor === "Dictionary" && true) {
    let items = __x__.__args[0];
    return (_P_ => Functor_p327.map()(Dictionary)(List.mapM(Functor_p327)(Applicative_p327)((__x__ => ((__x__) => {
  if (__x__.length === 2 && true && true) {
    let [k,v] = __x__;
    return Functor_p327.map()((mapped => ([k, mapped])))(f(v));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__)))(_P_)))(items);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(dict));
export let mapWithKey = (fn => __x__ => ((__x__) => {
  if (__x__.__constructor === "Dictionary" && true) {
    let items = __x__.__args[0];
    return (_P_ => Dictionary(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((i => ([Tuple.fst(i), fn(Tuple.fst(i))(Tuple.snd(i))])))(_P_)))(items);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
export let keys = (m => ((__x__) => {
  if (__x__.__constructor === "Dictionary" && true) {
    let items = __x__.__args[0];
    return Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(Tuple.fst)(items);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(m));
export let values = (m => ((__x__) => {
  if (__x__.__constructor === "Dictionary" && true) {
    let items = __x__.__args[0];
    return Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(Tuple.snd)(items);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(m));
export default { fromList, empty, insert, update, get, merge, _$_length_$_, toList, mapM, mapWithKey, keys, values };
