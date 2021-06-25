const os = require("os");
const { Binary } = require("binary-install");

function getPlatform() {
  const type = os.type();
  const arch = os.arch();

  if (type === "Windows_NT" && arch === "x64") return "win64";
  if (type === "Windows_NT") return "win32";
  if (type === "Linux" && arch === "x64") return "linux";
  if (type === "Darwin" && arch === "x64") return "macos";
  if (type === "Darwin" && arch === "arm64") return "macos";

  throw new Error(`Unsupported platform: ${type} ${arch}`);
}

function getBinary() {
  const platform = getPlatform();
  const version = require("../package.json").version;
  const url = `https://github.com/madlib-lang/madlib/releases/download/v${version}/madlib-${platform}.tar.gz`;
  const name = "madlib";

  return new Binary(name, url);
}

module.exports = getBinary;
