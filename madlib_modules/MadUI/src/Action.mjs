// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadUI/src/Action.mad
import {} from "./../../../__internals__.mjs"
import {  } from "./../../../.prelude/Wish.mjs";
import {  } from "./Event.mjs";

export let GlobalAction = (a => b => ({ __constructor: "GlobalAction", __args: [ a, b ] }));
Inspect['GlobalAction_8fa092d0d5414d48db6f0f118b65d141'] = {};
Inspect['GlobalAction_8fa092d0d5414d48db6f0f118b65d141']['inspect'] = () => (Inspect_g110) => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "GlobalAction" && true && true) {
    let a0 = __x__.__args[0];
    let a1 = __x__.__args[1];
    return `GlobalAction(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.a_arr_b.inspect()(a1) + `)`;
  }
  else if (true) {
    return `Unknown`;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__$a__));
export let syncAction = (stateUpdate => _ => event => ({ v: Monad.Wish_f76eea3ecc45547ab1e69e479c05b8d2.of()((state => stateUpdate(state)(event))), n: null }));
export let onAfterPrint = GlobalAction(`afterprint`);
export let onBeforePrint = GlobalAction(`beforeprint`);
export let onBeforeUnload = GlobalAction(`beforeunload`);
export let onDocumentLoad = GlobalAction(`load`);
export let onPageHide = GlobalAction(`pagehide`);
export let onPageShow = GlobalAction(`pageshow`);
export let onOffline = GlobalAction(`offline`);
export let onOnline = GlobalAction(`online`);
export let onResize = GlobalAction(`resize`);
export let onBodyScroll = GlobalAction(`scroll`);
export let onStorage = GlobalAction(`storage`);
export let onUnload = GlobalAction(`unload`);
export let onUrlChanged = GlobalAction(`popstate`);
export default { syncAction, onAfterPrint, onBeforePrint, onBeforeUnload, onDocumentLoad, onPageHide, onPageShow, onOffline, onOnline, onResize, onBodyScroll, onStorage, onUnload, onUrlChanged, GlobalAction };
