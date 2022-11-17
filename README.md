![build](https://github.com/madlib-lang/madlib/workflows/build/badge.svg)
[![codecov](https://codecov.io/gh/madlib-lang/madlib/branch/master/graph/badge.svg?token=4L2SE3380B)](https://codecov.io/gh/madlib-lang/madlib)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](code_of_conduct.md)

[![Discord](https://img.shields.io/discord/530598289813536771?label=discord%20madlib%20channel)](https://discord.com/channels/530598289813536771/932841899914571787)

# madlib
> madlib is a general purpose language that compiles to native binaries and Javascript.

## How to install
There are currently two ways to install the madlib compiler.

### Npm package
You can install it globally with the command `npm i -g @madlib-lang/madlib`. You can then compile code in the following way:
```
madlib -i "entryFile.mad" -o "outputFolder"
```

### Download the release assets
You can also download the archive of the build directly from the releases [releases](https://github.com/madlib-lang/madlib/releases) and install it wherever you want in your filesystem. You would then need to make sure that the location is in your PATH environment variable to make it available from everywhere.

## Features and Ideology

*madlib* shares much of its syntax / ideology with JavaScript. Atop the "good stuff", it introduces functional programing concepts from other functional programming languages including:
 - [algebraic data types](#example-type-checking)
 - [function composition](#example-function-composition)
 - [pattern matching](#example-pattern-matching)

### Type-checking

*madlib* does static type checking at compilation time using an approach based upon the [Hindley-Milner W algorithm](https://boxbase.org/entries/2018/mar/5/hindley-milner/). (Type annotations are also possible, but mostly not needed.)

### Variables

Variables in *madlib* may only be defined once and cannot be re-assigned. All data in *madlib* is immutable. Instead of picking between the different semantics / rules of `var` / `let` / `const` in JavaScript, in *madlib* you can eschew all three:

```madlib
x = 3
y = x + 1
user = { name: "Max" }
```

### Expressions

1. Every expression in *madlib* must return a value.
2. `null` / `undefined` are not valid keywords.
#### Examples
```madlib
x + 1

1

"Hello world"

// else is mandatory as the expression must return a value
if (a % 2 == 0) { "even" } else { "odd" }

where(maybe) {
  Just(x) =>
    x

  Nothing =>
    "Hello world"
}
```

### Functions

Functions are the heart of *madlib*.

1. A function is an expression which can be re-evaluated given contextual parameters.
2. A function must always return a value.
3. A function may only define a single expression.

#### Defining a function:
```
inc = (x) => x + 1
```

#### Typing a function:
```
inc :: Integer -> Integer
inc = (x) => x + 1
```

#### Evaluating a function
```
inc(3) // 4
```

#### Composing functions

The special pipe expression, which returns a function.

```madlib
compute :: Float -> Float
compute = pipe(
  inc,
  add(10),
  divide(2)
)
```

Alternatively you can find use the `|>` or pipeline operator to partially apply values or compose functions, left to right.

```
3 |> inc
// equivalent to:
inc(3)

3 |> inc |> inc
// equivalent to
inc(inc(3))
```



#### Currying
All functions are curried, therefore you can always partially apply them:
```
add = (a, b) => a + b

addFive = add(5)

17 |> addFive // 22
```

### Conditions

The keywords `if` / `else` are bound expressions in *madlib* and must return a value. The `else` case _must be defined_ in order to be valid.

Some examples:

```
if (true) { "Yes" } else { "No" }
if (cost > wallet) { goHome("With my little money") } else { watchShow("And enjoy it") }
```

Because it is an expression, we can directly pipe to whatever it returns:

```
if (true) { "Yes" } else { "No" }
  |> IO.log
```

Two shorthand syntaxes are also available and the above could then also be written like this:
```
// Without brackets:
(if (true) "Yes" else "No")
  |> IO.log

// Ternary:
(true ? "Yes" : "No")
  |> IO.log
```
NB: note that parenthesis are necessary around the expressions then, otherwise the piped `IO.Log` would be applied to the "No" string.

#### Type annotations

Because of *madlib*'s type inference, in the majority of cases you do not need to provide type annotations. However, if needed, you can explicitly define type annotations in the form of `(expression :: type)`:

```madlib
(1 :: Integer)     // here the annotation says that 1 is a Integer
(1 + 1 :: Integer) // here the annotation says that 1 + 1 is a Integer
(1 :: Integer) + 1 // here the annotation says that the first 1 is a Integer, and tells the type checker to infer the type of the second value
("Madlib" :: String)
("Madlib" :: Boolean) // Type error, "Madlib" should be a Boolean
```

### Algebraic Data Types

*madlib* allows for algebraic data types in the form of:

```madlib
type Maybe a
  = Just(a)
  | Nothing
```

Here `Maybe a` is the type. This type has a variable, that means that a `Maybe` can have different shapes and contain any other type.

`Just(a)` and `Nothing` are constructors of the type `Maybe`. They allow us to create values with that type. `type Maybe a = Just a | Nothing` generates these constructor functions for us.

Here is the type above in use:
```madlib
might = Just("something") // Maybe String
nope  = Nothing           // Maybe a
```

### Pattern matching

Pattern matching is a powerful tool for specifying what to do in a given function or [Record](#records).

For functions:
```madlib
type User
  = LoggedIn(String)
  | Anonymous

userDisplayName = (u) => where(u) {
  LoggedIn(name) =>
    name

  Anonymous =>
    "Anonymous"
}

```

For [Records](#records):
```madlib
getStreetName :: { address :: { street :: String } } -> String
getStreetName = (profile) => where(profile) {
  { address: { street } } =>
    street

  _ =>
    "Unknown address"
}
```
Note that you can use where without parameter, in which case it returns a function that takes whatever is matched as a parameter. So the above can be shortened like this:
```madlib
getStreetName :: { address :: { street :: String } } -> String
getStreetName = where {
  { address: { street } } =>
    street

  _ =>
    "Unknown address"
}
```

### Records
*madlib* offers a special `Record` type. A `Record` is analogous to a JavaScript object. It is a syntax for defining a custom shape for your data. A `Record`'s keys are identifiers and values can be any type. Here are examples:

```
language = { name: "Madlib", howIsIt: "cool" }
```

It can be used as constructor arguments by using Record types:
```
type User = LoggedIn({ name :: String, age :: Integer, address :: String })
```

It can be used in patterns:
```
user = LoggedIn({ name: "John", age: 33, address: "Street" })

where(user) {
  LoggedIn({ name: "John" }) =>
    "Hey John !!"

  _ =>
    "It's not John"
}
```

Records can be updated:
```
position = { x: 3, y: 7, z: -1 }
updatedPosition = { ...position, z: 1 }
```

### Modules
In *madlib* your code is organized in modules.
* A module is simply a source file.
* A module can export functions or can import functions from other modules. To do this, a module can export any top level assignment.

Right now the entrypoint module that you give to the compiler is the reference and its path defines the root path for your modules.

#### Named imports

Given the following structure:
```
src/Main.mad
   /Dependency.mad
```

```
// Dependency.mad
export someFn = (a) => ...
```

```
// Main.mad
import { someFn } from "./Dependency"

someFn(...)
```

Subfolders can be used to group related modules together. If we add one, it ends up with the current structure:

```
src/Main.mad
   /Dependency.mad
   /Sub/SubDependency.mad
```

Then we could have the modules defined like this:
```
// Sub/SubDependency.mad
export someSubFn = (a) => ...
```
```
// Main.mad
import { someSubFn } from "./Sub/SubDependency"

someSubFn(...)
```

#### Default imports
All exported names are automatically added to a default export that can then be imported as a default import in order to avoid naming collisions. For example, when importing `map` from the standard List module, you can do it like this:
```
import List from 'List'

List.map((x) => (x * 2), [1, 2, 3])
```

### Packages
A package is a way to share code across projects or create libraries. A package must have a madlib.json file with a following structure:
```json
{
  "name": "MadUI",
  "version": "0.0.1",
  "madlibVersion": "0.11.0", // the minimum madlib version required by the project
  "main": "src/Main.mad", // you must define the main module of your package
  "dependencies": [ // dependencies of your package
    {
      "url": "http://some.dep.url.zip",
      "description": "...",
      "minVersion": "0.1.0",
      "maxVersion": "2.0.7"
    }
  ]
}
```
The main module must export every name that you want to share.

### Examples


#### Example: Hello World

```
import IO from "IO"

IO.putLine("Hello World !")
```

## build

`stack build`

## run

```
./scripts/run run "examples/HelloWorld.mad"
node build/HelloWorld.mjs
```

## Contributing

### Setup
Your system should be setup with the following:
- [Stack](https://docs.haskellstack.org/en/stable/README/) and [GHC](https://www.haskell.org/ghc/) `8.10.7`
- [Node.js](https://nodejs.org/) ( > v14 is recommended )
- [Rollup](https://rollupjs.org/) with the package `@rollup/plugin-node-resolve` installed globally if you want to enable bundling or use the build script located at `/scripts/build`

### Scripts

#### build

This script builds a local version of madlib as well as dependencies that are
built in Madlib itself. Currently these are located in tools and are composed
of the test runner and the package downloader.

To run it:
```bash
./scripts/build
```

#### install

This runs a npm global install ( `npm link` effectively ) of the local package. Beware that the version used
in the package.json there needs to be a github release that is published in this
repository. You can have a look here to find out which versions are available:
[https://github.com/madlib-lang/madlib/releases](https://github.com/madlib-lang/madlib/releases).

To run it:
```bash
./scripts/install
```

#### update-pkg-build

This script allows you to swap the Madlib binary, Prelude Madlib files, and the tools
with the currently compiled version ( using the `build` script ). Then the latest
local build becomes globally available if you rely on it for other local projects
outside the directory of Madlib itself.

To run it:
```bash
./scripts/update-pkg-build
```

#### run

A wrapper to stack run, the usage is the same as Madlib executable itself but uses
the locally build one.

To run it:
```bash
./scripts/run --help
./scripts/run compile -i INPUT [OPTIONS]
```
