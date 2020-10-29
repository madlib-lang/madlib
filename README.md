![build](https://github.com/open-sorcerers/madlib/workflows/build/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/open-sorcerers/madlib/badge.svg?branch=master)](https://coveralls.io/github/open-sorcerers/madlib?branch=master)

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

```javascript
data User
  = LoggedIn String
  | Anonymous

userDisplayName = (u) => switch(u) {
  case LoggedIn name: name
  case Anonymous    : "Anonymous"
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
switch(us) {
  case LoggedIn { name: "John" }: "Hey John !!"
  case _                        : "It's not John"
}
```

## build

`stack build`

## run

`stack run < fixtures/example.mad`
`cat fixtures/example.mad | stack run`
