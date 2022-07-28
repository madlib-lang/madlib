// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadUI/src/Subscription.mad
import {} from "./../../../__internals__.mjs"
export let every = (ms => fn => dispatch => {
  setInterval(() => { dispatch(fn) }, ms);
});
export default { every };
