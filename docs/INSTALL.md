# Installing Madlib


## Via npm (recommended)

The easiest way to install Madlib is through npm. This downloads a prebuilt binary for your
platform automatically.

```sh
npm install -g @madlib-lang/madlib
```

After installation:
```sh
madlib --version
```


## Via GitHub release

Download a prebuilt archive for your platform from the
[GitHub releases page](https://github.com/madlib-lang/madlib/releases), extract it anywhere
on your filesystem, and add its location to your `PATH`.


## Building from source

Building from source requires:
- [Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
- Clang / LLVM (for the LLVM backend and runtime)
- `llvm-ar` (usually ships with LLVM)
- `libuv`, `libgc` (Boehm GC), `libpcre2`, `libcurl` (runtime dependencies)

### 1. Build the compiler

```sh
stack build
```

### 2. Build the runtime

Set `TARGET` to match your platform (see the table below) and run:

```sh
TARGET=MACOS_ARM64 AR=llvm-ar ./scripts/build-runtime
```

Supported targets:

| `TARGET` value | Platform |
|---|---|
| `MACOS_ARM64` | Apple Silicon (M1/M2/M3) |
| `MACOS_X64` | Intel Mac |
| `LINUX_ARM64` | ARM Linux |
| `LINUX_X64` | x86-64 Linux (glibc) |
| `MUSL_X64` | x86-64 Linux (musl / Alpine) |
| `WIN_X64` | x86-64 Windows |

You can override the C++ compiler with the `CXX` environment variable (default: `clang++`).

### 3. Install globally

Copy the freshly-built compiler and runtime into the npm package location so the `madlib`
command picks up your changes:

```sh
./scripts/update-pkg-build
```


## Running tests

Unit tests (Haskell):
```sh
stack test
```

Prelude / standard-library tests (requires a full build first):
```sh
cd prelude/__internal__ && madlib test -t llvm --O3 --coverage
```
