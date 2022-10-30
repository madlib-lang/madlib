// file: /opt/hostedtoolcache/node/14.20.1/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/List.mad
import {} from "./../__internals__.mjs"
import {  } from "./Monoid.mjs";
import {  } from "./Show.mjs";
import {  } from "./Monad.mjs";
import Fun from "./Function.mjs";
import { MORE } from "./Compare.mjs";
import { Just, Nothing } from "./Maybe.mjs";

Functor['List_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Functor['List_5b7ebeeaa5acfe1eeea5a9e9845b152d']['map'] = () => (f => list => {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$f = f;
    let $$list = list;

    while($_continue_) {
      let $f = $$f;
      let $list = $$list;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ !== null && true && true) {
    let { v: a, n: xs } = __x__;
    ($_end_ = $_end_.n = { v: $f(a) }, $$f = $f, $$list = xs, $_continue_ = true);
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
Applicative['List_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Applicative['List_5b7ebeeaa5acfe1eeea5a9e9845b152d']['ap'] = () => (mf => ma => (_P_ => flatten(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((f => Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(f)(ma)))(_P_)))(mf));
Applicative['List_5b7ebeeaa5acfe1eeea5a9e9845b152d']['pure'] = () => (x => ({ v: x, n: null }));
Monad['List_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Monad['List_5b7ebeeaa5acfe1eeea5a9e9845b152d']['chain'] = () => (f => xs => (_P_ => flatten(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(f)(_P_)))(xs));
Monad['List_5b7ebeeaa5acfe1eeea5a9e9845b152d']['of'] = () => Applicative.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.pure();
Semigroup['List_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Semigroup['List_5b7ebeeaa5acfe1eeea5a9e9845b152d']['assoc'] = () => (list1 => list2 => {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$list1 = list1;
    let $$list2 = list2;

    while($_continue_) {
      let $list1 = $$list1;
      let $list2 = $$list2;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ !== null && true && true) {
    let { v: item, n: more } = __x__;
    ($_end_ = $_end_.n = { v: item }, $$list1 = more, $$list2 = $list2, $_continue_ = true);
  }
  else if (__x__ === null) {
    ($_end_.n = $list2, $_result_ = $_start_.n);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})($list1)
    }
    return $_result_;
});
Monoid['List_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Monoid['List_5b7ebeeaa5acfe1eeea5a9e9845b152d']['mconcat'] = () => Semigroup.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.assoc();
Monoid['List_5b7ebeeaa5acfe1eeea5a9e9845b152d']['mempty'] = () => (null);
export let mapMaybe = (f => list => {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$f = f;
    let $$list = list;

    while($_continue_) {
      let $f = $$f;
      let $list = $$list;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ !== null && true && true) {
    let { v: a, n: xs } = __x__;
    ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let mapped = __x__.__args[0];
    ($_end_ = $_end_.n = { v: mapped }, $$f = $f, $$list = xs, $_continue_ = true);
  }
  else if (__x__.__constructor === "Nothing") {
    ($$f = $f, $$list = xs, $_continue_ = true);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})($f(a));
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
export let repeat = (a => count => {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$a = a;
    let $$count = count;

    while($_continue_) {
      let $a = $$a;
      let $count = $$count;

        $_continue_ = false;
        ($count <= 0 ? ($_end_.n = (null), $_result_ = $_start_.n) : ($_end_ = $_end_.n = { v: $a }, $$a = $a, $$count = ($count - 1), $_continue_ = true))
    }
    return $_result_;
});
export let repeatWith = (f => count => {
    let helper = (index => {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$index = index;

    while($_continue_) {
      let $index = $$index;

        $_continue_ = false;
        ($index >= count ? ($_end_.n = (null), $_result_ = $_start_.n) : ($_end_ = $_end_.n = { v: f($index) }, $$index = ($index + 1), $_continue_ = true))
    }
    return $_result_;
});
    return helper(0);
});
export let range = (start => end => repeatWith((i => (i + start)))((end - start)));
export let mapM = (Functor_z207) => (Applicative_z207) => (f => list => {
    let helper = (x => result => (_P_ => (__$PH2__ => Applicative_z207.ap()(__$PH2__)(result))((__$PH1__ => Functor_z207.map()(__$PH1__)(f(x)))(_P_)))((x_ => result_ => ({ v: x_, n: result_ }))));
    return reduceRight(helper)(Applicative_z207.pure()((null)))(list);
});
export let singleton = Applicative.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.pure();
export let intercalate = (sep => list => {
    let helper = (acc => l => {
    let $_result_;
    let $_continue_ = true;
    let $$acc = acc;
    let $$l = l;

    while($_continue_) {
      let $acc = $$acc;
      let $l = $$l;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ !== null && true && __x__.n === null) {
    let { v: a } = __x__;
    ($_result_ = append(a)($acc));
  }
  else if (__x__ === null) {
    ($_result_ = $acc);
  }
  else if (__x__ !== null && true && true) {
    let { v: a, n: xs } = __x__;
    ($$acc = concat($acc)(({ v: a, n: sep })), $$l = xs, $_continue_ = true);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})($l)
    }
    return $_result_;
});
    return helper((null))(list);
});
export let intersperse = (a => xs => ((__x__) => {
  if (__x__ === null) {
    return (null);
  }
  else if (__x__ !== null && true && __x__.n === null) {
    let { v: one } = __x__;
    return ({ v: one, n: null });
  }
  else if (__x__ !== null && true && __x__.n !== null && true && __x__.n.n === null) {
    let { v: one, n: { v: two } } = __x__;
    return ({ v: one, n: { v: a, n: { v: two, n: null } } });
  }
  else if (__x__ !== null && true && true) {
    let { v: one, n: rest } = __x__;
    return ({ v: one, n: { v: a, n: intersperse(a)(rest) } });
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(xs));
let _intercalateWithIndex = (i => f => xs => ((__x__) => {
  if (__x__ === null) {
    return (null);
  }
  else if (__x__ !== null && true && __x__.n === null) {
    let { v: one } = __x__;
    return ({ v: one, n: null });
  }
  else if (__x__ !== null && true && __x__.n !== null && true && __x__.n.n === null) {
    let { v: one, n: { v: two } } = __x__;
    return ({ v: one, n: { v: f(i), n: { v: two, n: null } } });
  }
  else if (__x__ !== null && true && true) {
    let { v: one, n: rest } = __x__;
    return ({ v: one, n: { v: f(i), n: _intercalateWithIndex((i + 1))(f)(rest) } });
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(xs));
export let intercalateWithIndex = _intercalateWithIndex(0);
export let mapWithIndex = (f => list => {
    let helper = (list_ => index => {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$list_ = list_;
    let $$index = index;

    while($_continue_) {
      let $list_ = $$list_;
      let $index = $$index;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ === null) {
    ($_end_.n = (null), $_result_ = $_start_.n);
  }
  else if (__x__ !== null && true && true) {
    let { v: a, n: xs } = __x__;
    ($_end_ = $_end_.n = { v: f(a)($index) }, $$list_ = xs, $$index = ($index + 1), $_continue_ = true);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})($list_)
    }
    return $_result_;
});
    return helper(list)(0);
});
export let concat = Semigroup.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.assoc();
export let append = (item => list => {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$item = item;
    let $$list = list;

    while($_continue_) {
      let $item = $$item;
      let $list = $$list;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ === null) {
    ($_end_.n = ({ v: $item, n: null }), $_result_ = $_start_.n);
  }
  else if (__x__ !== null && true && true) {
    let { v: a, n: xs } = __x__;
    ($_end_ = $_end_.n = { v: a }, $$item = $item, $$list = xs, $_continue_ = true);
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
export let last = (list => {
    let $_result_;
    let $_continue_ = true;
    let $$list = list;

    while($_continue_) {
      let $list = $$list;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ !== null && true && __x__.n === null) {
    let { v: item } = __x__;
    ($_result_ = Just(item));
  }
  else if (__x__ === null) {
    ($_result_ = Nothing);
  }
  else if (__x__ !== null && true && __x__.n !== null && true && true) {
    let { n: { v: a, n: xs } } = __x__;
    ($$list = ({ v: a, n: xs }), $_continue_ = true);
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
export let first = (list => ((__x__) => {
  if (__x__ === null) {
    return Nothing;
  }
  else if (__x__ !== null && true && true) {
    let { v: a,  } = __x__;
    return Just(a);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(list));
export let init = (list => {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$list = list;

    while($_continue_) {
      let $list = $$list;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ !== null && true && __x__.n === null) {
    let { v: item } = __x__;
    ($_end_.n = (null), $_result_ = $_start_.n);
  }
  else if (__x__ === null) {
    ($_end_.n = (null), $_result_ = $_start_.n);
  }
  else if (__x__ !== null && true && true) {
    let { v: a, n: xs } = __x__;
    ($_end_ = $_end_.n = { v: a }, $$list = xs, $_continue_ = true);
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
export let tail = (list => ((__x__) => {
  if (__x__ === null) {
    return (null);
  }
  else if (__x__ !== null && true && true) {
    let { v: a, n: xs } = __x__;
    return xs;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(list));
export let nth = (i => list => {
    let $_result_;
    let $_continue_ = true;
    let $$i = i;
    let $$list = list;

    while($_continue_) {
      let $i = $$i;
      let $list = $$list;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ === null) {
    ($_result_ = Nothing);
  }
  else if (__x__ !== null && true && true) {
    let { v: a, n: xs } = __x__;
    (__eq__($i, 0) ? ($_result_ = Just(a)) : ($$i = ($i - 1), $$list = xs, $_continue_ = true));
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
export let reduceRight = (f => acc => list => (_P_ => reduceLeft((a => b => f(b)(a)))(acc)(reverse(_P_)))(list));
export let reduceLeft = (f => acc => list => {
    let $_result_;
    let $_continue_ = true;
    let $$f = f;
    let $$acc = acc;
    let $$list = list;

    while($_continue_) {
      let $f = $$f;
      let $acc = $$acc;
      let $list = $$list;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ === null) {
    ($_result_ = $acc);
  }
  else if (__x__ !== null && true && true) {
    let { v: a, n: xs } = __x__;
    ($$f = $f, $$acc = $f($acc)(a), $$list = xs, $_continue_ = true);
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
export let reduce = reduceLeft;
export let reduceM = (Functor_v515) => (Applicative_v515) => (Monad_v515) => (f => acc => list => ((__x__) => {
  if (__x__ === null) {
    return Monad_v515.of()(acc);
  }
  else if (__x__ !== null && true && true) {
    let { v: a, n: xs } = __x__;
    return Monad_v515.chain()((v => reduceM(Functor_v515)(Applicative_v515)(Monad_v515)(f)(v)(xs)))(f(acc)(a));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(list));
export let filter = (predicate => list => {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$predicate = predicate;
    let $$list = list;

    while($_continue_) {
      let $predicate = $$predicate;
      let $list = $$list;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ === null) {
    ($_end_.n = (null), $_result_ = $_start_.n);
  }
  else if (__x__ !== null && true && true) {
    let { v: a, n: xs } = __x__;
    ($predicate(a) ? ($_end_ = $_end_.n = { v: a }, $$predicate = $predicate, $$list = xs, $_continue_ = true) : ($$predicate = $predicate, $$list = xs, $_continue_ = true));
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
export let reject = (predicate => list => filter(Fun.complement(predicate))(list));
export let find = (predicate => list => {
    let $_result_;
    let $_continue_ = true;
    let $$predicate = predicate;
    let $$list = list;

    while($_continue_) {
      let $predicate = $$predicate;
      let $list = $$list;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ === null) {
    ($_result_ = Nothing);
  }
  else if (__x__ !== null && true && true) {
    let { v: a, n: xs } = __x__;
    ($predicate(a) ? ($_result_ = Just(a)) : ($$predicate = $predicate, $$list = xs, $_continue_ = true));
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
export let _$_length_$_ = (list => {
    let helper = (list_ => count => {
    let $_result_;
    let $_continue_ = true;
    let $$list_ = list_;
    let $$count = count;

    while($_continue_) {
      let $list_ = $$list_;
      let $count = $$count;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ === null) {
    ($_result_ = $count);
  }
  else if (__x__ !== null && true && true) {
    let { v: a, n: xs } = __x__;
    ($$list_ = xs, $$count = ($count + 1), $_continue_ = true);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})($list_)
    }
    return $_result_;
});
    return helper(list)(0);
});
export let slice = (start => end => list => {
    let helper = (start_ => end_ => list_ => {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$start_ = start_;
    let $$end_ = end_;
    let $$list_ = list_;

    while($_continue_) {
      let $start_ = $$start_;
      let $end_ = $$end_;
      let $list_ = $$list_;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ === null) {
    ($_end_.n = (null), $_result_ = $_start_.n);
  }
  else if (__x__ !== null && true && true) {
    let { v: a, n: xs } = __x__;
    (__eq__($start_, 0) && $end_ > 0 ? ($_end_ = $_end_.n = { v: a }, $$start_ = 0, $$end_ = ($end_ - 1), $$list_ = xs, $_continue_ = true) : ($start_ > 0 ? ($$start_ = ($start_ - 1), $$end_ = ($end_ - 1), $$list_ = xs, $_continue_ = true) : ($_end_.n = (null), $_result_ = $_start_.n)));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})($list_)
    }
    return $_result_;
});
    let realStart = (start < 0 ? (start + _$_length_$_(list)) : start);
    let realEnd = (__eq__(end, 0) ? _$_length_$_(list) : (end < 0 ? (end + _$_length_$_(list)) : end));
    return helper(realStart)(realEnd)(list);
});
export let isEmpty = (xs => __eq__(xs, (null)));
export let uniqueBy = (f => reduce((result => elem => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    return result;
  }
  else if (__x__.__constructor === "Nothing") {
    return append(elem)(result);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(find(f(elem))(result))))((null)));
let descending = (compareFn => a => as => xs => {
    let $_result_;
    let $_continue_ = true;
    let $$compareFn = compareFn;
    let $$a = a;
    let $$as = as;
    let $$xs = xs;

    while($_continue_) {
      let $compareFn = $$compareFn;
      let $a = $$a;
      let $as = $$as;
      let $xs = $$xs;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ !== null && true && true) {
    let { v: b, n: bs } = __x__;
    (__eq__($compareFn($a)(b), MORE) ? ($$compareFn = $compareFn, $$a = b, $$as = ({ v: $a, n: $as }), $$xs = bs, $_continue_ = true) : ($_result_ = ({ v: ({ v: $a, n: $as }), n: sequences($compareFn)($xs) })));
  }
  else if (true) {
    ($_result_ = ({ v: ({ v: $a, n: $as }), n: sequences($compareFn)($xs) }));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})($xs)
    }
    return $_result_;
});
let ascending = (compareFn => a => as => xs => {
    let $_result_;
    let $_continue_ = true;
    let $$compareFn = compareFn;
    let $$a = a;
    let $$as = as;
    let $$xs = xs;

    while($_continue_) {
      let $compareFn = $$compareFn;
      let $a = $$a;
      let $as = $$as;
      let $xs = $$xs;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ !== null && true && true) {
    let { v: b, n: bs } = __x__;
    (!__eq__($compareFn($a)(b), MORE) ? ($$compareFn = $compareFn, $$a = b, $$as = (ys => $as(({ v: $a, n: ys }))), $$xs = bs, $_continue_ = true) : ($_result_ = ({ v: $as(({ v: $a, n: null })), n: sequences($compareFn)($xs) })));
  }
  else if (true) {
    ($_result_ = ({ v: $as(({ v: $a, n: null })), n: sequences($compareFn)($xs) }));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})($xs)
    }
    return $_result_;
});
let sequences = (compareFn => list => ((__x__) => {
  if (__x__ !== null && true && __x__.n !== null && true && true) {
    let { v: a, n: { v: b, n: xs } } = __x__;
    return (__eq__(compareFn(a)(b), MORE) ? descending(compareFn)(b)(({ v: a, n: null }))(xs) : ascending(compareFn)(b)((l => ({ v: a, n: l })))(xs));
  }
  else if (true) {
    let xs = __x__;
    return ({ v: xs, n: null });
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(list));
export let sortBy = (compareFn => list => {
    let merge = (listA => listB => {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$listA = listA;
    let $$listB = listB;

    while($_continue_) {
      let $listA = $$listA;
      let $listB = $$listB;

        $_continue_ = false;
        ((__x__) => {
  if (__x__.length === 2 && __x__[0] !== null && true && true && __x__[1] !== null && true && true) {
    let [{ v: a, n: as },{ v: b, n: bs }] = __x__;
    (__eq__(compareFn(a)(b), MORE) ? ($_end_ = $_end_.n = { v: b }, $$listA = $listA, $$listB = bs, $_continue_ = true) : ($_end_ = $_end_.n = { v: a }, $$listA = as, $$listB = $listB, $_continue_ = true));
  }
  else if (__x__.length === 2 && __x__[0] === null && true) {
    let [,bs] = __x__;
    ($_end_.n = bs, $_result_ = $_start_.n);
  }
  else if (__x__.length === 2 && true && __x__[1] === null) {
    let [as,] = __x__;
    ($_end_.n = as, $_result_ = $_start_.n);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([$listA, $listB]))
    }
    return $_result_;
});
    let mergePairs = (l => {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$l = l;

    while($_continue_) {
      let $l = $$l;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ !== null && true && __x__.n !== null && true && true) {
    let { v: a, n: { v: b, n: xs } } = __x__;
    ($_end_ = $_end_.n = { v: merge(a)(b) }, $$l = xs, $_continue_ = true);
  }
  else if (true) {
    let xs = __x__;
    ($_end_.n = xs, $_result_ = $_start_.n);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})($l)
    }
    return $_result_;
});
    let mergeAll = (l => {
    let $_result_;
    let $_continue_ = true;
    let $$l = l;

    while($_continue_) {
      let $l = $$l;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ !== null && true && __x__.n === null) {
    let { v: x } = __x__;
    ($_result_ = x);
  }
  else if (true) {
    let xs = __x__;
    ($$l = mergePairs(xs), $_continue_ = true);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})($l)
    }
    return $_result_;
});
    return (_P_ => mergeAll(sequences(compareFn)(_P_)))(list);
});
export let sort = (Comparable_i944) => sortBy(Comparable_i944.compare());
export let sortAsc = (Comparable_l947) => sort(Comparable_l947);
export let sortDesc = (Comparable_r953) => sortBy((a => b => (Comparable_r953.compare()(a)(b) * -1)));
export let flatten = reduceLeft(concat)((null));
export let zip = (as => bs => {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$as = as;
    let $$bs = bs;

    while($_continue_) {
      let $as = $$as;
      let $bs = $$bs;

        $_continue_ = false;
        ((__x__) => {
  if (__x__.length === 2 && __x__[0] !== null && true && true && __x__[1] !== null && true && true) {
    let [{ v: a, n: aa },{ v: b, n: bb }] = __x__;
    ($_end_ = $_end_.n = { v: ([a, b]) }, $$as = aa, $$bs = bb, $_continue_ = true);
  }
  else if (__x__.length === 2 && __x__[0] !== null && true && __x__[0].n === null && __x__[1] !== null && true && __x__[1].n === null) {
    let [{ v: a },{ v: b }] = __x__;
    ($_end_.n = ({ v: ([a, b]), n: null }), $_result_ = $_start_.n);
  }
  else if (__x__.length === 2 && __x__[0] === null && __x__[1] === null) {
    let [,] = __x__;
    ($_end_.n = (null), $_result_ = $_start_.n);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([$as, $bs]))
    }
    return $_result_;
});
export let includes = (x => list => {
    let $_result_;
    let $_continue_ = true;
    let $$x = x;
    let $$list = list;

    while($_continue_) {
      let $x = $$x;
      let $list = $$list;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ === null) {
    ($_result_ = false);
  }
  else if (__x__ !== null && true && true) {
    let { v: a, n: xs } = __x__;
    (__eq__(a, $x) ? ($_result_ = true) : ($$x = $x, $$list = xs, $_continue_ = true));
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
export let drop = (n => list => slice(n)(0)(list));
export let dropLast = (n => list => slice(0)(-n)(list));
export let take = (n => list => slice(0)(n)(list));
export let takeLast = (n => list => slice(-n)(0)(list));
export let dropWhile = (predicate => list => {
    let $_result_;
    let $_continue_ = true;
    let $$predicate = predicate;
    let $$list = list;

    while($_continue_) {
      let $predicate = $$predicate;
      let $list = $$list;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ === null) {
    ($_result_ = (null));
  }
  else if (__x__ !== null && true && true) {
    let { v: a, n: xs } = __x__;
    ($predicate(a) ? ($$predicate = $predicate, $$list = xs, $_continue_ = true) : ($_result_ = ({ v: a, n: xs })));
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
export let takeWhile = (predicate => list => {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$predicate = predicate;
    let $$list = list;

    while($_continue_) {
      let $predicate = $$predicate;
      let $list = $$list;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ === null) {
    ($_end_.n = (null), $_result_ = $_start_.n);
  }
  else if (__x__ !== null && true && true) {
    let { v: a, n: xs } = __x__;
    ($predicate(a) ? ($_end_ = $_end_.n = { v: a }, $$predicate = $predicate, $$list = xs, $_continue_ = true) : ($_end_.n = (null), $_result_ = $_start_.n));
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
export let reverse = (list => {
    let helper = (acc => l => {
    let $_result_;
    let $_continue_ = true;
    let $$acc = acc;
    let $$l = l;

    while($_continue_) {
      let $acc = $$acc;
      let $l = $$l;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ !== null && true && true) {
    let { v: h, n: xs } = __x__;
    ($$acc = ({ v: h, n: $acc }), $$l = xs, $_continue_ = true);
  }
  else if (__x__ === null) {
    ($_result_ = $acc);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})($l)
    }
    return $_result_;
});
    return helper((null))(list);
});
export default { mapMaybe, repeat, repeatWith, range, mapM, singleton, intercalate, intersperse, intercalateWithIndex, mapWithIndex, concat, append, last, first, init, tail, nth, reduceRight, reduceLeft, reduce, reduceM, filter, reject, find, _$_length_$_, slice, isEmpty, uniqueBy, sortBy, sort, sortAsc, sortDesc, flatten, zip, includes, drop, dropLast, take, takeLast, dropWhile, takeWhile, reverse };
