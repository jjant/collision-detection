const { Elm } = require("./ConfigSchema.js");

const app = Elm.ConfigSchema.init();

app.ports.generateFile.subscribe(([a, b]) => {
  console.log(a, b);
});
