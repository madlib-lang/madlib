// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Header.mad
import {} from "./../../../../__internals__.mjs"
import { JS, LLVM } from "./../State.mjs";
import { button, className, div, onClick, onInput, input, placeholder, InputEvent, inputType, h1, header, syncAction } from "./../../../MadUI/src/Main.mjs";
import String from "./../../../../.prelude/String.mjs";
import { always } from "./../../../../.prelude/Function.mjs";

let handleInput = (state => event => ((__x__) => {
  if (__x__.__constructor === "InputEvent" && true) {
    let e = __x__.__args[0];
    return ({ v: Monad.Wish_48091bbb4c188d584814a4a3f8207f71.of()(always(({ ...state, search: String.toLower(e.target.value) }))), n: null });
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(event));
let handleTargetChange = (target => syncAction((state => _ => ({ ...state, target: target }))));
export let Header = (target => header(({ v: className(`header`), n: null }))(({ v: h1(({ v: className(`header__title`), n: null }))(({ v: `MadDoc`, n: null })), n: { v: input(({ v: inputType(`text`), n: { v: placeholder(`What are you looking for?`), n: { v: className(`search-field`), n: { v: onInput(handleInput), n: null } } } }))((null)), n: { v: div(({ v: className(`target-selector`), n: null }))(({ v: button(({ v: className(`target-selector__button` + (__eq__(target, JS) ? ` target-selector__button--selected` : ``)), n: { v: onClick(handleTargetChange(JS)), n: null } }))(({ v: `Javascript`, n: null })), n: { v: button(({ v: className(`target-selector__button` + (__eq__(target, LLVM) ? ` target-selector__button--selected` : ``)), n: { v: onClick(handleTargetChange(LLVM)), n: null } }))(({ v: `LLVM`, n: null })), n: null } })), n: null } } })));
export default { Header };
