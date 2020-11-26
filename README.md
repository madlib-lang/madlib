![build](https://github.com/open-sorcerers/madlib/workflows/build/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/open-sorcerers/madlib/badge.svg?branch=master)](https://coveralls.io/github/open-sorcerers/madlib?branch=master)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](code_of_conduct.md) 

# madlib
> madlib is a general purpose language that compiles to Javascript.

## How to install
There are currently two ways to install the madlib compiler.

### Npm package
You can install it globally with the command `npm i -g @open-sorcerers/madlib`. You can then compile code in the following way:
```
madlib -i "entryFile.mad" -o "outputFolder"
```

### Download the release assets
You can also download the archive of the build directly from the releases [releases](https://github.com/open-sorcerers/madlib/releases) and install it wherever you want in your filesystem. You would then need to make sure that the location is in your PATH environment variable to make it available from everywhere.

## Features and Ideology

*madlib* shares much of its syntax / ideology with JavaScript. Atop the "good stuff", it introduces functional programing concepts from other functional programming languages including:
 - [algebraic data types](#example-type-checking)
 - [function composition](#example-function-composition)
 - [pattern matching](#example-pattern-matching)

### Type-checking

*madlib* does static type checking at compilation time using an approach based upon the [Hindley-Milner W algorithm](https://boxbase.org/entries/2018/mar/5/hindley-milner/). (Type annotations are also possible, but mostly not needed.)

### Variables

Variables in *madlib* may only be defined once and cannot be re-assigned. All data in *madlib* is immutable. Instead of picking between the different semantics / rules of `var` / `let` / `const` in JavaScript, in *madlib* you can eschew all three:

```
x = 3
y = x + 1
user = { name: "Max" }
```

### Expressions

1. Every expression in *madlib* must return a value.
2. `null` / `undefined` are not valid keywords.

```
x + 1
```

### Functions

Functions are the heart of *madlib*.

1. A function is an expression which can be re-evaluated given contextual parameters.
2. A function must always return a value.
3. A function may only define a single expression.

#### Defining a function:
```
inc = (x) => (x + 1)
```

#### Typing a function:
```
inc :: Number -> Number
inc = (x) => (x + 1)
```

#### Evaluating a function
```
inc(3) // 4
```

#### Composing functions

We use the `|>` or pipeline operator to partially apply values or compose functions, left to right.

```
3 |> inc // 4
// equivalent to:
inc(3) // 4
3 |> inc |> inc // 5
// equivalent to
inc(inc(3)) // 5
```

#### Currying
All functions are curried, therefore you can always partially apply them:
```
add = (a, b) => (a + b)

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

```
(1 :: Number)     // here the annotation says that 1 is a Number
(1 + 1 :: Number) // here the annotation says that 1 + 1 is a Number
(1 :: Number) + 1 // here the annotation says that the first 1 is a Number, and tells the type checker to infer the type of the second value
("Madlib" :: String)
("Madlib" :: Boolean) // Type error, "Madlib should be a Boolean"
```

### Algebraic Data Types

*madlib* allows for algebraic data types in the form of:

```
data Maybe a = Just a | Nothing
```

Here `Maybe a` is the type. This type has a variable, that means that a `Maybe` can have different shapes and contain any other type.

`Just a` and `Nothing` are constructors of the type `Maybe`. They allow us to create values with that type. `data Maybe a = Just a | Nothing` generates these constructor functions for us.

Here is the type above in use:
```
might = Just("something") // Maybe String
nope  = Nothing           // Maybe a
```

### Pattern matching

Pattern matching is a powerful tool for specifying what to do in a given function or [Record](#records).

For functions:
```
data User
  = LoggedIn String
  | Anonymous

userDisplayName = (u) => (
  where(u) {
    is LoggedIn name: name
    is Anonymous    : "Anonymous"
  }
)
```

For [Records](#records):
```
getStreetName :: { address: { street: String } }
getStreetName = (p1, p2) => (
  where({ p1: p1, p2: p2 }) {
    is { address: { street: s } }: s
    is _                         : "Unknown address"
  }
)
```

### Records
*madlib* offers a special `Record` type. A `Record` is analogous to a JavaScript object. It is a syntax for defining a custom shape for your data. A `Record`'s keys are identifiers and values can be any type. Here are examples:

```
language = { name: "Madlib", howIsIt: "cool" }
```

It can be used as constructor arguments by using Record types:
```
data User = LoggedIn { name :: String, age :: Number, address :: String }
```

It can be used in patterns:
```
user = LoggedIn({ name: "John", age: 33, address: "Street" })

where(us) {
  is LoggedIn { name: "John" }: "Hey John !!"
  is _                        : "It's not John"
}
```

As with JavaScript objects, records can be spread:
```
position2D = { x: 3, y: 7 }
position3D = { ...position2D, z: 1 }
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
import L from 'List'

L.map((x) => (x * 2), [1, 2, 3])
```

### Packages

Coming soon.

### Examples


#### Example: Hello World

```
import IO from "IO"

IO.log("Hello World !")
```

## build

`stack build`

## run

```
stack run -- -i "fixtures/example.mad"
node build/fixtures/example.mjs
```
