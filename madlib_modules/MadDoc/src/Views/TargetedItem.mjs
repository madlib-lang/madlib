// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/TargetedItem.mad
import {} from "./../../../../__internals__.mjs"
import {  } from "./../../../MadUI/src/Main.mjs";
import {  } from "./../TargetInfo.mjs";
import { JSTarget, BothTargets, LLVMTarget } from "./../Parser/Documentation.mjs";
import { LLVM, JS } from "./../State.mjs";

Inspect['Record_hasJSf_059c0648b0200e5d96a19498b14e81296_hasLLVMf_159c0648b0200e5d96a19498b14e81296_isAvailablef_259c0648b0200e5d96a19498b14e81296'] = {};
Inspect['Record_hasJSf_059c0648b0200e5d96a19498b14e81296_hasLLVMf_159c0648b0200e5d96a19498b14e81296_isAvailablef_259c0648b0200e5d96a19498b14e81296']['inspect'] = () => (Inspect_t97) => (Inspect_m90) => (Inspect_f83) => (__$a__ => `{ ` + `hasJS: ` + Inspect_f83.inspect()(__$a__.hasJS) + `, ` + `hasLLVM: ` + Inspect_m90.inspect()(__$a__.hasLLVM) + `, ` + `isAvailable: ` + Inspect_t97.inspect()(__$a__.isAvailable) + ` }`);
export let TargetedItem = (target => targeted => cardView => ((__x__) => {
  if (__x__.__constructor === "JSTarget" && true) {
    let js = __x__.__args[0];
    return (__eq__(target, JS) ? cardView(({ hasJS: true, hasLLVM: false, isAvailable: true }))(js) : cardView(({ hasJS: true, hasLLVM: false, isAvailable: false }))(js));
  }
  else if (__x__.__constructor === "LLVMTarget" && true) {
    let llvm = __x__.__args[0];
    return (__eq__(target, LLVM) ? cardView(({ hasJS: false, hasLLVM: true, isAvailable: true }))(llvm) : cardView(({ hasJS: false, hasLLVM: true, isAvailable: false }))(llvm));
  }
  else if (__x__.__constructor === "BothTargets" && true && true) {
    let js = __x__.__args[0];
    let llvm = __x__.__args[1];
    return (__eq__(target, JS) ? cardView(({ hasJS: true, hasLLVM: true, isAvailable: true }))(js) : cardView(({ hasJS: true, hasLLVM: true, isAvailable: true }))(llvm));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(targeted));
export default { TargetedItem };
