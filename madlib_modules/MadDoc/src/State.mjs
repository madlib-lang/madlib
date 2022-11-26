// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/State.mad
import {} from "./../../../__internals__.mjs"
import {  } from "./Parser/Documentation.mjs";

export let LLVM = ({ __constructor: "LLVM", __args: [  ] });
export let JS = ({ __constructor: "JS", __args: [  ] });
Inspect['Target_80f4a43bf97979d3e4a8a496cd64fec2'] = {};
Inspect['Target_80f4a43bf97979d3e4a8a496cd64fec2']['inspect'] = () => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "LLVM") {
    return `LLVM`;
  }
  else if (__x__.__constructor === "JS") {
    return `JS`;
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
Inspect['Record_modulesf_080f4a43bf97979d3e4a8a496cd64fec2_pathf_180f4a43bf97979d3e4a8a496cd64fec2_searchf_280f4a43bf97979d3e4a8a496cd64fec2_targetf_380f4a43bf97979d3e4a8a496cd64fec2'] = {};
Inspect['Record_modulesf_080f4a43bf97979d3e4a8a496cd64fec2_pathf_180f4a43bf97979d3e4a8a496cd64fec2_searchf_280f4a43bf97979d3e4a8a496cd64fec2_targetf_380f4a43bf97979d3e4a8a496cd64fec2']['inspect'] = () => (Inspect_v99) => (Inspect_o92) => (Inspect_h85) => (Inspect_a78) => (__$a__ => `{ ` + `modules: ` + Inspect_a78.inspect()(__$a__.modules) + `, ` + `path: ` + Inspect_h85.inspect()(__$a__.path) + `, ` + `search: ` + Inspect_o92.inspect()(__$a__.search) + `, ` + `target: ` + Inspect_v99.inspect()(__$a__.target) + ` }`);
export default { LLVM, JS };
