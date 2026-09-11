const test = require("node:test");
const assert = require("node:assert/strict");
const { getTriple, localBuildBinary } = require("./get-binary");

test("resolves every published npm binary target", () => {
  assert.equal(getTriple("Windows_NT", "x64", false), "x86_64-pc-windows-mingw32");
  assert.equal(getTriple("Darwin", "x64", false), "x86_64-apple-darwin");
  assert.equal(getTriple("Darwin", "arm64", false), "arm64-apple-darwin");
  assert.equal(getTriple("Linux", "x64", false), "x86_64-unknown-linux-gnu");
  assert.equal(getTriple("Linux", "x64", true), "x86_64-alpine-linux-musl");
});

test("rejects unsupported npm binary targets", () => {
  assert.equal(getTriple("Linux", "arm64", false), null);
});

test("does not mistake a missing Stack directory for a local compiler", () => {
  assert.equal(localBuildBinary("/path/that/does/not/exist"), null);
});
