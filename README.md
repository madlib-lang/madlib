![build](https://github.com/open-sorcerers/madlib/workflows/build/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/open-sorcerers/madlib/badge.svg?branch=master)](https://coveralls.io/github/open-sorcerers/madlib?branch=master)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](code_of_conduct.md) 

# madlib

## Description
Madlib is a general purpose language that compiles to Javascript. It shares a lot of its syntax with javascript. Trying to make it as Javascript as possible, it introduces functional programing concepts from other functional programing langugages such as algebraic data types, function composition, pattern matching with as much simplicity and javascriptcity as possible.


## Type checking
It does static type checking at compilation time based on Hindley Milner W algorithm. Type annotations are also possible, but mostly not needed. 

## hello world

```javascript
import IO from "IO"

IO.log("Hello World !")
```

## Languages features

### Small note on expressions
Every expression in Madlib must and does return a value, null or undefined are not allowed.

### Variables
Variables can't be introduced twice in a scope, or reassigned. In short, every data you manipulate in Madlib is immutable. The syntax is the same as the one of javascript, except that you do not need `var` `let` or `const` keywords. Here are a few examples:

```javascript
a = 3
b = a + 1
user = { name: "Max" }
```

### Functions
Functions are the heart of Madlib. They ressemble a lot of the ones of Javascript, except that they must always return something, and only one expression per function is allowed. Although that might sound restrictive, there are mechanism such as pattern matching and function composition that allow us to get more out of it than it sounds like.

Defining a function:
```javascript
inc = (x) => x + 1
```

Annotating type of a function:
```javascript
inc :: Num -> Num
inc = (x) => x + 1
```

Calling a function:
```javascript
inc(3) // 4
```

Composing functions:
```javascript
3 |> inc |> inc // 5
```

### Type annotations
You can provide type annotations for any Madlib function in the following way:
```javascript
inc :: Num -> Num
inc = (x) => x + 1
```

You may also, even though mostly not necessary thanks to type inference, provide type annotations for any expression with the following syntax: `(exp :: type)`
```javascript
(1 :: Num)     // here the annotation says that 1 is a Num
(1 + 1 :: Num) // here the annotation says that 1 + 1 is a Num
(1 :: Num) + 1 // here the annotation says that the first 1 is a Num, and lets the type checker figure out what the type of the second one is.

("Madlib" :: String)
("Madlib" :: Bool) // Type error, "Madlib should be a Bool"
```

### Currying
All functions are curried, therefore you can always partially apply them:
```javascript
add = (a, b) => a + b

addFive = add(5)

17 |> addFive // 22
```

### Conditions
As most languages, Madlib offers an if/else statement, except that it is an expression and thus must return a value. That is why it must always define an else case. Some examples:

```javascript
if (True) { "Yes" } else { "No" }
```

Because it is an expression, we can directly pipe to whatever it returns:

```javascript
if (True) { "Yes" } else { "No" }
  |> IO.log
```

### Data types
With Madlib we can define algebraic data types. The syntax there being pretty close to the one of Haskell, but fear not, once understood it gets easy to read, here are some examples:

Here `Maybe a` is the type. This type has a variable, that means that a Maybe can have different shapes and contain any other type.

`Just a` and `Nothing` are constructors of the type Maybe. They allow us to create values with that type, and therefore the following definition generates these constructor functions for us.
```javascript
data Maybe a = Just a | Nothing
```

From the definition above here is how we can create a Maybe:
```javascript
might = Just("something") // Maybe String
nope  = Nothing           // Maybe a
```

### Pattern matching
We can also match variable values given some pattern that match type constructors:

For data types:
```javascript
data User
  = LoggedIn String
  | Anonymous

userDisplayName = (u) => where(u) {
  is LoggedIn name: name
  is Anonymous    : "Anonymous"
}
```

For records:
```javascript
getStreetName :: { address: { street: String } }
getStreetName = (p1, p2) => where({ p1: p1, p2: p2 }) {
  is { address: { street: s } }: s
  is _                         : "Unknown address"
}
```

### Records
Madlib offers a special `Record` type. A Record is analog to a Javascript Object. It allows to define a custom shape for your data. Keys are identifiers and values can be any type. Here are examples:

```javascript
language = { name: "Madlib", howIsIt: "cool" }
```

It can be used as constructor arguments by using Record types:
```javascript
data User = LoggedIn { name :: String, age :: Num, address :: String }
```

It can be used in patterns:
```javascript
user = LoggedIn({ name: "John", age: 33, address: "Street" })

where(us) {
  is LoggedIn { name: "John" }: "Hey John !!"
  is _                        : "It's not John"
}
```

Like for Javascript objects, records can be spread:
```javascript
position2D = { x: 3, y: 7 }
position3D = { ...position2D, z: 1 }
```


### Modules
In Madlib you can organize your code in modules. A module is simply a source file. A module can export functions or can import functions from other modules. To do this, a module can export any top level assignment.

Right now the entrypoint module that you give to the compiler is the reference and its path defines the root path for your modules.

Given the following structure:
```
src/Main.mad
   /Dependency.mad
```
You should define your modules like this:
```javascript
// Dependency.mad
export someFn = (a) => ...

// Main.mad
import { someFn } from "Dependency"

someFn(...)
```

It is also possible to create sub folders in order to group related modules together. Let's add one and we end up with the current structure:

```
src/Main.mad
   /Dependency.mad
   /Sub/SubDependency.mad
```

Then we could have the modules defined like this:
```javascript
// Sub/SubDependency.mad
export someSubFn = (a) => ...

// Main.mad
import { someSubFn } from "Sub/SubDependency"

someSubFn(...)
```

## build

`stack build`

## run

`stack run "fixtures/example.mad"`
`node build/fixtures/example.mjs`
