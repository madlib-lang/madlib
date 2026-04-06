# Madlib Language Server

The Madlib compiler includes a built-in Language Server Protocol (LSP) implementation
that provides IDE features for `.mad` files.


## Starting the Language Server

```bash
madlib lsp
```

The server communicates over stdin/stdout using the standard LSP protocol (JSON-RPC).


## Editor Setup

### VS Code

Use a generic LSP client extension (e.g. `vscode-languageclient`) configured to
run `madlib lsp` as the server command. The server expects to be launched from the
project root (where `madlib.json` lives).

### Other Editors

Any editor that supports LSP can use the Madlib language server. Configure it to:
- **Command:** `madlib lsp`
- **Working directory:** the project root (containing `madlib.json`)
- **File types:** `*.mad`
- **Communication:** stdin/stdout


## Supported Features (19 capabilities)

### Core Editing

| Feature | LSP Method | Description |
|---------|-----------|-------------|
| **Diagnostics** | `textDocument/publishDiagnostics` | Real-time error and warning reporting as you type (100ms debounce) |
| **Hover** | `textDocument/hover` | Type signatures, documentation, and `@param`/`@returns` doc tags |
| **Go to Definition** | `textDocument/definition` | Jump to the definition of any symbol, including cross-module |
| **Find References** | `textDocument/references` | Find all usages of a symbol across the entire project |
| **Rename** | `textDocument/rename` | Rename a symbol across all files with a single operation |
| **Signature Help** | `textDocument/signatureHelp` | Function parameter hints while typing arguments |

### Navigation

| Feature | LSP Method | Description |
|---------|-----------|-------------|
| **Document Symbols** | `textDocument/documentSymbol` | Outline view of all top-level symbols in the current file |
| **Workspace Symbols** | `workspace/symbol` | Fuzzy search for symbols across all compiled project modules |
| **Folding Ranges** | `textDocument/foldingRange` | Code folding for blocks, functions, type definitions, and imports |
| **Call Hierarchy** | `textDocument/prepareCallHierarchy` | View incoming and outgoing calls for a function (custom method) |

### Code Intelligence

| Feature | LSP Method | Description |
|---------|-----------|-------------|
| **Autocompletion** | `textDocument/completion` | Context-aware completions: variables, types, constructors, module members, record fields |
| **Inlay Hints** | `textDocument/inlayHint` | Inline type annotations for let bindings and function parameters (custom method) |
| **Semantic Tokens** | `textDocument/semanticTokens/full` | Rich syntax highlighting: constructors, types, variables, methods (custom method) |
| **Code Actions** | `textDocument/codeAction` | Quick fixes: remove unused imports, add missing pattern branches, add missing imports |

### Note on Custom Methods

Call hierarchy, inlay hints, and semantic tokens are implemented as custom LSP methods
(`SCustomMethod`). Most modern editors (VS Code, Neovim with lsp-config, etc.) support
these automatically. Older clients may need explicit configuration.


## Diagnostics

The language server provides rich diagnostics with:

- **Context-specific error titles** — e.g. "Cannot use '+' with String — did you mean '<>'?"
  instead of the generic "Type error"
- **Smart hints** — actionable suggestions based on the specific error and types involved.
  For example, using `+` on strings suggests `<>` for concatenation.
- **Code examples** — showing the correct syntax for common mistakes
  (mutation with `:=`, deriving instances, etc.)
- **Stdlib-aware suggestions** — when you use an unimported name like `map`, the error
  says: `'map' is defined in the 'List' module. Add: import List from "List"`
- **Did-you-mean** — typo detection using edit distance for variables, types, and exports
- **Dual-target checking** — both JS and LLVM type-checking passes run, catching all
  possible errors regardless of your compilation target


## Cache Invalidation

When a file changes, the language server:

1. Invalidates the Rock cache for the changed file and its reverse dependencies
2. Re-type-checks the changed file (both JS and LLVM targets)
3. Compares the file's exported interface (types, functions) against the previous snapshot
4. If the interface changed, re-checks **all known project modules** so that diagnostics
   for dependent files are updated
5. Publishes fresh diagnostics for all affected files

This means fixing a type error in a library module will automatically clear errors
in files that import it — no need to touch those files.


## Architecture

The language server is split into focused modules:

| Module | Responsibility |
|--------|---------------|
| `Run.LanguageServer` | Server setup, handler dispatch, background compilation |
| `Run.LanguageServer.State` | `State` type, task runners, shared utilities |
| `Run.LanguageServer.Diagnostics` | Error/warning formatting, diagnostic publishing, cache invalidation |
| `Run.LanguageServer.Hover` | Hover info, node finding, type display |
| `Run.LanguageServer.Completion` | Autocompletion suggestions and context detection |

Error formatting is handled by:

| Module | Responsibility |
|--------|---------------|
| `Explain.Format` | Diagnostic report builders (rich and simple), JSON error output |
| `Explain.Format.Hints` | Smart hints: operator hints, stdlib suggestions, grammar error patterns |
| `Explain.Format.TypeDiff` | Type rendering with color-coded diffs for expected/found mismatches |


## Startup Sequence

1. The server initializes two `Driver.State` instances (one for JS target, one for LLVM)
2. On `initialized`, a background thread discovers all project modules from `madlib.json`
3. Modules are type-checked in batches (with concurrency) to populate the Rock cache
4. Once complete, features like workspace symbols, find references, and call hierarchy
   become available across all modules
5. A log message is sent: `"Background compilation complete: N modules"`


## Limitations

- Background compilation of all project modules happens on startup. Some features
  (workspace symbols, find references, call hierarchy) are unavailable until it completes.
- The server uses **full document sync** — each change sends the entire file content.
  This is reliable but uses more bandwidth than incremental sync.
- Call hierarchy, inlay hints, and semantic tokens use custom LSP methods rather than
  the standard protocol extensions. Most editors handle this transparently.
