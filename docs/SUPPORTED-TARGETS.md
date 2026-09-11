# Supported targets

Madlib has three compiler targets. A target is supported only when it is built
and tested by the release process described below.

| Target | Status | Notes |
| --- | --- | --- |
| Node.js | Supported | Default JavaScript target; exercised by compiler and prelude tests. |
| LLVM native | Supported | Requires the bundled runtime and is exercised on Linux x64. |
| Browser | Experimental | JavaScript generation is available, but Node-specific I/O, process, and thread APIs are unavailable. |

## npm compiler binaries

Published binaries currently support Windows x64, macOS x64/arm64, Linux x64 with
glibc, and Linux x64 with musl. Unsupported hosts fail before any download.
Every release archive is accompanied by an `<archive>.sha256` asset. The npm
installer downloads and verifies this SHA-256 digest before extracting the
archive.

## Compatibility rule

New standard-library APIs must either have equivalent Node and LLVM behavior
with tests on both targets, or be target-gated and documented as experimental.
Browser support must not be advertised for an API that depends on the Node or
native runtime.
