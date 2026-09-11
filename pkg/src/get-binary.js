const os = require("os");
const { existsSync, readdirSync } = require("fs");
const { join } = require("path");
const { VerifiedBinary } = require("./verified-binary");

function isMusl() {
  const report = process.report && process.report.getReport && process.report.getReport();
  return Boolean(report && report.header && !report.header.glibcVersionRuntime);
}

function getTriple(type = os.type(), arch = os.arch(), musl = isMusl()) {
  if (type === "Windows_NT" && arch === "x64") return "x86_64-pc-windows-mingw32";
  if (type === "Darwin" && arch === "x64") return "x86_64-apple-darwin";
  if (type === "Darwin" && arch === "arm64") return "arm64-apple-darwin";
  if (type === "Linux" && arch === "x64") return musl ? "x86_64-alpine-linux-musl" : "x86_64-unknown-linux-gnu";
  return null;
}

// A source checkout has a compiler built by Stack. Prefer it so `madlib` from
// a linked/local npm package never reaches out to GitHub (and can be used
// before a matching release has been published).
function localBuildBinary(root = join(__dirname, "..", "..")) {
  const dist = join(root, ".stack-work", "dist");
  if (!existsSync(dist)) return null;
  try {
    for (const platform of readdirSync(dist)) {
      const platformPath = join(dist, platform);
      for (const ghc of readdirSync(platformPath)) {
        const binary = join(platformPath, ghc, "build", "madlib", process.platform === "win32" ? "madlib.exe" : "madlib");
        if (existsSync(binary)) return binary;
      }
    }
  } catch (_) {
    // A partially-created Stack directory is not a local build.
  }
  return null;
}

function getBinary() {
  const triple = getTriple();
  if (!triple) {
    throw new Error(`Unsupported Madlib platform: ${os.type()} ${os.arch()}. Supported platforms are Windows x64/arm64, and Linux x64 (glibc or musl).`);
  }
  const version = require("../package.json").version;
  const releaseBase = `https://github.com/madlib-lang/madlib/releases/download/v${version}`;
  const archive = `madlib-${triple}.tar.gz`;
  return new VerifiedBinary(
    "madlib",
    `${releaseBase}/${archive}`,
    `${releaseBase}/${archive}.sha256`,
    archive,
    localBuildBinary(),
  );
}

module.exports = getBinary;
module.exports.getTriple = getTriple;
module.exports.localBuildBinary = localBuildBinary;
