// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Main.mad
import {} from "./../../../__internals__.mjs"
import { renderWithConfig, DEFAULT_CONFIG, addGlobalEventHandler } from "./../../MadUI/src/Main.mjs";
import { DocApp, handleUrlChanged, initialState } from "./App.mjs";

let main = (_ => {
    renderWithConfig(addGlobalEventHandler(handleUrlChanged)(DEFAULT_CONFIG))(DocApp)(initialState)(`app`);
    return ({ __constructor: "Unit", __args: [] });
});
export default {};

main(null)
