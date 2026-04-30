const __setToArray__ = (s) => {
  let nextNodes = [s]
  let result = []

  while (nextNodes.length > 0) {
    const elem = nextNodes.shift();

    if (elem.__constructor === `SetRBNode`) {
      result = result.concat(__setToArray__(elem._2))
      result.push(elem._1)
      nextNodes.push(elem._3)
    }
  }

  return result;
}

const __dictToArray__ = (s) => {
  let nextNodes = [s]
  let result = []

  while (nextNodes.length > 0) {
    const elem = nextNodes.shift();

    if (elem.__constructor === `DictRBNode`) {
      result = result.concat(__dictToArray__(elem._3))
      result.push(elem._1)
      result.push(elem._2)
      nextNodes.push(elem._4)
    }
  }

  return result;
}

const __eq__Set = (l, r) => {
  const arrL = __setToArray__(l)
  const arrR = __setToArray__(r)

  let result = true
  let index = 0

  while (result && index < arrL.length) {
    if (!__eq__(arrL[index], arrR[index])) {
      result = false
    }

    index = index + 1
  }

  return result
}

const __eq__Dict = (l, r) => {
  const arrL = __dictToArray__(l)
  const arrR = __dictToArray__(r)

  let result = true
  let index = 0

  while (result && index < arrL.length) {
    if (!__eq__(arrL[index], arrR[index])) {
      result = false
    }

    index = index + 1
  }

  return result
}

global.__eq__ = (l, r) => {
  if (l === r) {
    return true;
  }
  if (typeof l !== typeof r) {
    return false;
  }
if (l === null && r !== null || l !== null && r === null) {
  return false;
}
if (l === null && r === null) {
  return true;
}
  if (typeof l === `object`) {
if (l.n && l.v) {
  let result = true;
  while (l !== null && result) {
    if (r === null) {
      result = false
      break
    }
    result = __eq__(l.v, r.v)
    l = l.n
    r = r.n
  }
  return result && r === null;
}

  if (l.__constructor === `SetRBNode` || l.__constructor === `SetRBEmpty`) {
    return __eq__Set(l, r)
  }
  if (l.__constructor === `DictRBNode` || l.__constructor === `DictRBEmpty`) {
    return __eq__Dict(l, r)
  }

    if (l.__constructor !== undefined) {
      // ADT fast path: slim layout puts arity in __a and args
      // in numbered fields _0.._n.  Skip the generic Object.keys
      // + reduce route once tags match.
      if (l.__constructor !== r.__constructor) return false;
      const n = l.__a;
      if (n !== r.__a) return false;
      for (let i = 0; i < n; i++) {
        const k = '_' + i;
        if (!__eq__(l[k], r[k])) return false;
      }
      return true;
    }

    const keysL = Object.keys(l);
    const keysR = Object.keys(r);
    return keysL.length === keysR.length && keysL.reduce((res, k) => res && __eq__(l[k], r[k]), true);
  }
  return l === r;
}

const __applyMany__ = (f, params) => params.reduce((_f, param) => _f(param), f);
global.__apMtdDicts__ = (dict, dicts) => {
  const o = {};
  for (const k of Object.keys(dict)) {
    o[k] = () => __applyMany__(dict[k](), dicts);
  }
  return o;
};

global.__once__ = (fn, context) => {

    var result;

    return function() {

        if (fn) {

            result = fn.apply(context || this, arguments);

            fn = null;

        }

        return result;

    };

}


global.__listToJSArray__ = (list) => {
  let res = []

  while (list) {
    res.push(list.v)
    list = list.n
  }

  return res
}

global.__jsArrayToList__ = (arr) => {
  let res = null

  for (let i = arr.length - 1; i >= 0; i--) {
    let head = { v: arr[i], n: res }
    res = head
  }

  return res
}

global.__listCtorSpread__ = (_spread, _next) => {
  if (_spread === null) {
    return _next
  }

  let head = { ..._spread }
  let result = head
  while (head.n !== null) {
    head = head.n
  }
  head.n = _next
  return result
}

