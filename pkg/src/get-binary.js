const os = require("os");
const { Binary } = require("binary-install");


function getTriple() {
  const type = os.type();
  const arch = os.arch();

  if (type === "Windows_NT" && arch === "x64") {
    return "x86_64-pc-windows-mingw32"
  }
  if (type === "Darwin" && arch === "x64") {
    return "x86_64-apple-darwin"
  }
  if (type === "Darwin" && arch === "arm64") {
    return "arm64-apple-darwin"
  }
  if (type === "Linux" && arch === "x64") {
    return "x86_64-unknown-linux-gnu"
  }
}

function getBinary() {
  const triple = getTriple();
  const version = require("../package.json").version;
  const url = `https://github.com/madlib-lang/madlib/releases/download/v${version}/madlib-${triple}.tar.gz`;
  const name = "madlib";

  return new Binary(name, url);
}

module.exports = getBinary;
