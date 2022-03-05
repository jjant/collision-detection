import "./style.css";
import { Elm } from "./src/Main.elm";
import { ElmConfigUi } from "./elm-config.js";

ElmConfigUi.init({
  // This is where you'll persist your config data for other users.
  // It's fine if this file has just an empty object for now, like "{}".
  filepath: "./config.json",
  localStorageKey: "my_app",
  callback: function (elmConfigUiData) {
    const root = document.querySelector("#app div");

    Elm.Main.init({
      node: root,
      flags: elmConfigUiData,
    });
  },
});
