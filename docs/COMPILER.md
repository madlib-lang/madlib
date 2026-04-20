# Compiler internals

This document describes the internal architecture of the Madlib compiler: how source files are
transformed into executables or JavaScript modules, what compilation passes exist, how
optimization levels affect the pipeline, and how coverage instrumentation works.


## Overview

The Madlib compiler is written in Haskell and uses an incremental, demand-driven build system
built on the [Rock](https://hackage.haskell.org/package/rock) library. Each compilation artifact
is described as a *query*; queries are memoized and only re-evaluated when their inputs change,
enabling fast incremental rebuilds in `--watch` mode.

Source files (`.mad`) are processed through a shared frontend (parsing, canonicalization, type
inference, monomorphization, and optimization) and then routed to either the LLVM backend or the
JavaScript backend depending on the `--target` flag.


## Compilation pipeline

```
.mad source files
      │
      ▼
┌─────────────┐
│   Lexing    │  (Parse/Lexer/) — tokenizes source into a token stream
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   Parsing   │  (Parse/Megaparsec/) — builds a source-level AST (Src.AST)
└──────┬──────┘
       │
       ▼
┌──────────────────────┐
│  Module resolution   │  resolves all imports transitively; detects import cycles
└──────────┬───────────┘
           │
           ▼
┌─────────────────────────────────────────────────┐
│               Canonicalization                  │
│  (Canonicalize/) — symbol resolution,           │
│  desugaring, target macro expansion,            │
│  coverage instrumentation (if --coverage),      │
│  AST rewriting (if optimization ≥ O2)           │
└──────────────────┬──────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────┐
│               Type inference                    │
│  (Infer/) — Hindley-Milner with type classes,   │
│  constraint solving, unification                │
└──────────────────┬──────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────┐
│             Monomorphization                    │
│  (Infer/Monomorphize.hs) — specializes all      │
│  polymorphic functions; eliminates interface    │
│  dictionaries entirely                          │
└──────────────────┬──────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────┐
│          Conversion to Core AST                 │
│  (Optimize/ToCore.hs) — flattens and            │
│  normalizes the AST for optimization passes     │
└──────────────────┬──────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────┐
│           Optimization passes                   │
│  (Optimize/) — level-dependent; see below       │
└────────────┬──────────────┬─────────────────────┘
             │              │
             ▼              ▼
   ┌──────────────┐  ┌──────────────────┐
   │ LLVM backend │  │ JavaScript backend│
   │              │  │                  │
   │ LLVM IR gen  │  │ JS codegen       │
   │ LLVM passes  │  │ Source maps      │
   │ Object files │  │ Bundling         │
   │ Linking      │  │                  │
   └──────┬───────┘  └────────┬─────────┘
          │                   │
          ▼                   ▼
   native executable     .mjs modules
```


## Frontend passes

### Lexing
**Location**: `compiler/main/Parse/Lexer/`

The lexer converts raw source bytes into a `TokenStream` using a hand-written scanner
(`scanMany`). This token stream is then fed to the parser.

### Parsing
**Location**: `compiler/main/Parse/Megaparsec/`

The parser is built with [Megaparsec](https://hackage.haskell.org/package/megaparsec) and
produces a source-level AST (`Src.AST`). It handles all language constructs including
expressions, declarations, type annotations, patterns, imports, and exports. Parse errors
are reported with structured source locations.

### Module resolution
**Location**: `compiler/main/Driver/Rules.hs` (`ModulePathsToBuild` query)

Starting from an entry-point file, the compiler recursively resolves all `import` statements
to file paths, transitively collecting every module that needs to be compiled. Import cycles
are detected and reported as errors at this stage.

### Canonicalization
**Location**: `compiler/main/Canonicalize/`

Transforms the source AST into a canonical AST (`Can.AST`):
- Resolves all names to their fully-qualified definitions.
- Desugars template strings, operator sections, and other syntactic sugar.
- Expands `#iftarget` / `#elseif` / `#endif` conditional compilation blocks.
- Applies AST-level rewrites when optimization level ≥ O2 (e.g. map fusion).
- Inserts coverage tracking nodes when `--coverage` is enabled (see [Coverage](#coverage)).

### Type inference
**Location**: `compiler/main/Infer/`

Implements Hindley-Milner type inference extended with type classes:
- Infers and propagates types through all expressions.
- Solves type class constraints and selects instances.
- Performs unification with Robinson's algorithm, including occurs-check.
- Reports type errors with source locations and suggestions.

Output is a fully-typed AST (`Slv.AST`) along with the type environment.

### Monomorphization
**Location**: `compiler/main/Infer/Monomorphize.hs`

Eliminates all polymorphism at compile time by generating a concrete, specialized copy of
every polymorphic function for each distinct set of type arguments it is called with.
Interface method calls are resolved statically to their concrete implementations. There are
**no interface dictionaries** and **no dynamic dispatch** at runtime — all of this is resolved
here. See [ABI.md](ABI.md) for details.


## Optimization passes

**Location**: `compiler/main/Optimize/`

After monomorphization the AST is lowered to a simpler Core representation, then a
series of optimization passes runs. Which passes execute depends on the optimization
level selected with `--O0` / `--O1` / `--O2` / `--O3`.

| Pass | O0 | O1 | O2 | O3 | Description |
|---|:---:|:---:|:---:|:---:|---|
| Rename | ✓ | ✓ | ✓ | ✓ | Alpha-rename bindings to avoid shadowing |
| Sort expressions | ✓ | ✓ | ✓ | ✓ | Topological sort of definitions |
| Tail call elimination (TCE) | | ✓ | ✓ | ✓ | Convert tail-recursive calls to loops |
| Simplify calls | | | ✓ | ✓ | Simplify known-arity function calls |
| Fold calls | | | ✓ | ✓ | Inline small call wrappers |
| AST rewriting (canonicalize) | | | ✓ | ✓ | E.g. map/filter fusion |
| Inlining | | | | ✓ | Inline small functions at call sites |
| Allocation sinking | | | | ✓ | Delay allocations to reduce live ranges |
| Escape analysis | | | | ✓ | Determine which values escape their scope |
| Higher-order copy propagation | | | | ✓ | Propagate known functions through HOFs |

Additionally, when targeting LLVM, closure conversion is always applied (regardless of
optimization level) to turn captured variables into explicit environment structs.

### Default optimization level

When no flag is given the default is **O1**.

### LLVM pass manager

On top of the Madlib-level passes, the LLVM backend runs the LLVM pass manager at the
corresponding LLVM optimization level (0–3). At O1 and above this includes:
- Inlining (threshold: 200 at O1–O3)
- Loop vectorization
- Superword-level parallelism (SLP vectorization)
- Library call simplification


## LLVM backend

**Location**: `compiler/main/Generate/LLVM/`

### Code generation

Each module is compiled independently to an LLVM IR module and then assembled into an
object file (`.o`). The key steps are:

1. **Symbol table construction** — imports are resolved into a symbol table mapping names
   to LLVM operands and metadata (arity, constructor info, etc.).
2. **ADT constructor generation** — constructor functions and their tags are emitted.
3. **Top-level function generation** — each definition becomes an LLVM function. All
   parameters and return values use `i8*` (boxed). See [ABI.md](ABI.md) for type
   representations.
4. **Object file emission** — the LLVM module is passed through the LLVM pass manager and
   emitted as a native object file.

### Debug symbols

Pass `--debug` to emit DWARF debug information. This sets the LLVM optimization level to 0
for the pass manager regardless of the `-O` flag, and reduces the inlining threshold.

### Emitting LLVM IR

Pass `--emit-llvm` to write a `.ll` IR file alongside each object file. Useful for
inspecting what the compiler generates.

### Linking

After all modules are compiled the object files are linked into a single executable using
the system linker. The runtime static library (`libruntime.a`) and its dependencies (Boehm
GC, libuv, libpcre2, libcurl, etc.) are linked in automatically.

Platform-specific linker invocations:
- **macOS**: `clang++` with `-flto -dead_strip -foptimize-sibling-calls`
- **Linux**: `g++` with `-static`
- **Windows**: `g++` with `-static`

The `LDFLAGS` environment variable can be used to pass additional linker flags.

### PGO (Profile-Guided Optimization)

The LLVM backend supports PGO in two steps:

1. **Instrument**: compile with `--pgo-instrument`. Produces a binary that writes a profile
   data file when run.
2. **Optimize**: compile with `--pgo-optimize <profile-file>`. Uses the collected profile to
   guide optimization decisions.

When PGO is active the compiler emits LLVM IR to a temporary file and invokes `clang++`
directly instead of the LLVM pass manager.


## JavaScript backend

**Location**: `compiler/main/Generate/Javascript/`

### Code generation

Each module is compiled to an ES module (`.mjs`). The JS generator walks the Core AST and
emits curried arrow functions, plain objects for records, `{ __constructor, __args }` objects
for ADT values, and linked-list nodes for List values. See [ABI.md](ABI.md) for the full
type mapping.

A small runtime helpers module (`__internals__.mjs`) is emitted alongside the compiled
modules. It contains the structural equality function (`__eq__`), list/array conversion
helpers, and other utilities required by compiled code.

### Source maps

| Flag | Behavior |
|---|---|
| `--source-maps` | Write a separate `.mjs.map` file alongside each compiled module |
| `--source-maps-inline` | Embed the source map inside the compiled module via `//# sourceMappingURL=...` |

### Bundling

Pass `--bundle` to combine all compiled modules into a single output file using esbuild.
Combine with `--optimize` to also minify the bundle.

### Targets

| Target | Description |
|---|---|
| `node` (default) | ES modules targeting Node.js |
| `browser` | ES modules targeting a browser environment |


## Coverage instrumentation

**Location**: `compiler/main/Canonicalize/Coverage.hs`

Enable with `--coverage` on either `madlib compile` or `madlib test`.

When coverage is enabled the canonicalization pass instruments source modules by inserting
tracking calls around:
- Top-level expressions (line coverage)
- Function bodies (function coverage)
- Branch points — `if`/`else`, `where` arms, ternary expressions (branch coverage)

The following modules are **not** instrumented: packages, test runner internals, builtins,
prelude internals, and the coverage modules themselves.

The entry-point module gets an additional import of `__Coverage__` and a `Process.onExit`
hook that writes the coverage report (in LCOV format) when the program exits.

Coverage reports are generated by the `__Coverage__.mad` and `__CoverageTracking__.mad`
modules in the prelude.


## Incremental compilation and watch mode

The compiler tracks a content hash for every source file. When a file changes (detected
via filesystem events in watch mode), only the queries that transitively depend on that
file are invalidated and re-evaluated. Everything else is served from the memo cache,
making incremental rebuilds fast.

Pass `--watch` (or `-w`) to `madlib compile`, `madlib test`, or `madlib run` to enable
watch mode.


## Error reporting

Parse errors, type errors, and other diagnostics are collected throughout the pipeline and
reported after compilation. Each error carries a source location (file, line, column range).

By default errors are formatted as human-readable text with suggestions and similar-name
hints for typos. Pass `--error-format json` to receive machine-readable JSON output (useful
for IDE integrations).


## CLI reference

### `madlib compile`

| Flag | Default | Description |
|---|---|---|
| `-i`, `--input` | required | Entry-point source file |
| `-o`, `--output` | `./build/` | Output directory |
| `-t`, `--target` | `node` | Target: `llvm`, `node`, or `browser` |
| `--O0` / `--O1` / `--O2` / `--O3` | O1 | Optimization level |
| `--debug` | false | Emit DWARF debug symbols (LLVM only) |
| `--coverage` | false | Instrument for coverage collection |
| `--emit-llvm` | false | Write `.ll` IR files (LLVM only) |
| `--source-maps` | off | Write external source map files (JS only) |
| `--source-maps-inline` | off | Embed source maps inline (JS only) |
| `--bundle` | false | Bundle output into a single file (JS only) |
| `--optimize` | false | Minify bundle (JS only, use with `--bundle`) |
| `--watch`, `-w` | false | Watch for file changes and recompile |
| `--pgo-instrument` | off | Instrument binary for PGO profiling (LLVM only) |
| `--pgo-optimize <file>` | off | Optimize using PGO profile data (LLVM only) |
| `--error-format` | `text` | Error format: `text` or `json` |
| `-c`, `--config` | `madlib.json` | Path to configuration file |
| `-v`, `--verbose` | false | Verbose output |

### `madlib test`

| Flag | Default | Description |
|---|---|---|
| `-i`, `--input` | `.` | Test entry point |
| `-t`, `--target` | `node` | Target: `llvm` or `node` |
| `--O0` / `--O1` / `--O2` / `--O3` | O1 | Optimization level |
| `--coverage` | false | Collect coverage during test run |
| `-s`, `--suite` | all | Run only suites whose path contains this substring |
| `-n`, `--test-index` | all | Run only the nth test within matching suites (0-based) |
| `--watch`, `-w` | false | Watch for file changes and re-run tests |
| `--debug` | false | Build with debug symbols (LLVM only) |
| `--error-format` | `text` | Error format: `text` or `json` |

### `madlib run`

| Flag | Default | Description |
|---|---|---|
| (positional) | required | Package or module to run |
| `-t`, `--target` | `node` | Target: `llvm` or `node` |
| `--exe-path` | `./build/run` | Path for compiled executable (LLVM only) |
| `--watch`, `-w` | false | Watch for file changes and re-run |

### Other commands

| Command | Description |
|---|---|
| `madlib install` | Install dependencies from `madlib.json` |
| `madlib add <url>` | Add a package dependency |
| `madlib remove <name>` | Remove a package dependency |
| `madlib new <folder>` | Scaffold a new project |
| `madlib format` | Format source files |
| `madlib doc` | Generate documentation |
| `madlib package` | Analyze and release a package (bumps version, writes `version.lock`) |
| `madlib lsp` | Start Language Server Protocol server |
| `madlib repl` | Start an interactive REPL |
| `madlib config` | Print installation paths |
