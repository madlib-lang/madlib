const test = require("node:test");
const assert = require("node:assert/strict");
const { checksumFor } = require("./verified-binary");

test("selects only the checksum for the requested archive", () => {
  const digest = "a".repeat(64);
  assert.equal(checksumFor(`${digest}  madlib-x86_64-unknown-linux-gnu.tar.gz\n`, "madlib-x86_64-unknown-linux-gnu.tar.gz"), digest);
  assert.equal(checksumFor(`${digest}  other.tar.gz\n`, "madlib-x86_64-unknown-linux-gnu.tar.gz"), null);
});
