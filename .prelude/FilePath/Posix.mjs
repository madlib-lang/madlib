// file: /opt/hostedtoolcache/node/14.20.1/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/FilePath/Posix.mad
import {} from "./../../__internals__.mjs"
import String from "./../String.mjs";
import {  } from "./../Char.mjs";
import { filter, first, last, drop, dropLast } from "./../List.mjs";
import { fromMaybe, Nothing, Just } from "./../Maybe.mjs";
import { complement, equals, identity, ifElse } from "./../Function.mjs";

export let dropTrailingPathSeparator = ifElse((path => !__eq__(path, `/`) && __eq__(String.lastChar(path), Just(__String.fromCharCode(47)))))(String.dropLast(1))(identity);
let performSplitPath = (buffer => foundSlash => path => {
    let $_result_;
    let $_continue_ = true;
    let $$buffer = buffer;
    let $$foundSlash = foundSlash;
    let $$path = path;

    while($_continue_) {
      let $buffer = $$buffer;
      let $foundSlash = $$foundSlash;
      let $path = $$path;

        $_continue_ = false;
        ((__x__) => {
  if (__x__.__constructor === "Nothing") {
    ($_result_ = ({ v: $buffer, n: null }));
  }
  else if (__x__.__constructor === "Just" && __x__.__args[0] === __String.fromCharCode(47)) {
    ($$buffer = $buffer + `/`, $$foundSlash = true, $$path = String.drop(1)($path), $_continue_ = true);
  }
  else if (__x__.__constructor === "Just" && true) {
    let char = __x__.__args[0];
    ($foundSlash ? ($_result_ = Monoid.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.mconcat()(({ v: $buffer, n: null }))(performSplitPath(Show.Char_5b7ebeeaa5acfe1eeea5a9e9845b152d.show()(char))(false)(String.drop(1)($path)))) : ($$buffer = $buffer + Show.Char_5b7ebeeaa5acfe1eeea5a9e9845b152d.show()(char), $$foundSlash = false, $$path = String.drop(1)($path), $_continue_ = true));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(String.firstChar($path))
    }
    return $_result_;
});
export let splitPath = performSplitPath(``)(false);
export let joinPath = (_P_ => ifElse((_P_ => equals(Just(`/`))(first(_P_))))((_P_ => Monoid.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.mconcat()(`/`)(String.join(`/`)(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(dropTrailingPathSeparator)(drop(1)(_P_))))))((_P_ => String.join(`/`)(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(dropTrailingPathSeparator)(_P_))))(filter(complement(String.isEmpty))(_P_)));
export let canonicalizePath = (_P_ => joinPath(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((_P_ => ifElse((_P_ => equals(Just(__String.fromCharCode(47)))(String.lastChar(_P_))))(String.replace(`([^/]+)/*`)(`$1/`))(identity)(ifElse((_P_ => equals(`./`)(String.take(2)(_P_))))(String.drop(2))(identity)(_P_))))(splitPath(_P_))));
export let dropPathSegments = (howMany => _P_ => joinPath(drop(howMany)(splitPath(_P_))));
export let parentPath = (_P_ => joinPath(dropLast(1)(splitPath(_P_))));
export let isRootPathOf = (root => path => {
    let $_result_;
    let $_continue_ = true;
    let $$root = root;
    let $$path = path;

    while($_continue_) {
      let $root = $$root;
      let $path = $$path;

        $_continue_ = false;
            let rootParts = splitPath($root);
    let pathParts = splitPath($path);
    let rootStart = dropTrailingPathSeparator(fromMaybe(``)(first(rootParts)));
    let pathStart = dropTrailingPathSeparator(fromMaybe(``)(first(pathParts)));
(__eq__(rootStart, pathStart) || __eq__(rootStart, ``) ? (__eq__(rootStart, ``) ? ($_result_ = true) : ($$root = dropPathSegments(1)($root), $$path = dropPathSegments(1)($path), $_continue_ = true)) : ($_result_ = false))
    }
    return $_result_;
});
export let takeFileName = (_P_ => (__x__ => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let part = __x__.__args[0];
    return (__eq__(String.lastChar(part), Just(__String.fromCharCode(47))) ? `` : part);
  }
  else if (__x__.__constructor === "Nothing") {
    return ``;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__))(last(splitPath(_P_))));
export let takeExtension = (_P_ => fromMaybe(``)(Functor.Maybe_eadd07e55d46112f77467431f86f5e2d.map()(ifElse(String.isEmpty)(identity)(Monoid.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.mconcat()(`.`)))(last(String.split(`.`)(takeFileName(_P_))))));
export default { dropTrailingPathSeparator, splitPath, joinPath, canonicalizePath, dropPathSegments, parentPath, isRootPathOf, takeFileName, takeExtension };
