const { Elm } = require("/tmp/ConfigSchema.js");
const fs = require("fs");
const app = Elm.ConfigSchema.init();

app.ports.generateFile.subscribe(([fileName, fileContents]) => {
  fs.writeFileSync(fileName, fileContents);
});
