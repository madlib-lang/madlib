# Migrating to *madlib* from JavaScript
## Common Pitfalls & Delights

*madlib* is a constructed language which is intended to make the syntax and semantics of
JavaScript more palatable and expressive, while restricting certain constructs.

1. [Types](#types)
2. [The Fence](#the-fence)
3. [Literals](#literals)
   - [Booleans](#booleans)
   - [Strings](#strings)
   - [Numbers](#numbers)
   - Regular Expressions — not currently supported without fencing
4. [Variables](#variables)
5. [Objects / Records](#objects--records)
6. [Expressions](#expressions)
7. [Functions](#functions)
   - [Single expression per function](#single-expression-per-function)
   - [Auto-curry](#auto-curry)
   - [Pattern matching](#pattern-matching)
   - [Implicit returns](#implicit-returns)
8. [Constructors](#constructors)
9. [Modules](#modules)


# Types

One of the essential utilities of *madlib* is its type inference system. Where possible you
should use its types to provide correctness and enable features that only work with
well-typed code.

## Global types

| Madlib | JavaScript equivalent |
|---|---|
| `String` | string |
| `Integer` | number (whole) |
| `Float` | number (decimal) |
| `Boolean` | boolean |
| `List a` | array (linked-list at runtime) |
| `{}` (Unit) | void / undefined |

## Defining your own types

```madlib
type Color = Red | Green | Blue

type Maybe a
  = Just(a)
  | Nothing
```

See [TYPES.md](TYPES.md) for a full reference.


# The Fence

In the case that you want to do something in *madlib* which is not yet expressible in the
current native *madlib* syntax, simply wrap a valid JS expression with either the
single-line fence:
```madlib
#- expression -#
```
or a multi-line fence:
```madlib
#- {
  // JavaScript
} -#
```

There is a downside to doing this: content within the fence is opaque to *madlib*'s type
inference. Use fences sparingly and always annotate the surrounding function with an explicit
type signature.

To write code that behaves differently on each backend use conditional compilation:
```madlib
#iftarget js

toLower :: String -> String
export toLower = (s) => #- { return s.toLowerCase() } -#

#elseif llvm

toLower :: String -> String
export toLower = extern "madlib__string__toLower"

#endif
```


# Literals

Much of *madlib*'s syntax is identical to JavaScript, but there are a few discrepancies.


# Variables

Instead of choosing between `var` / `let` / `const`, *madlib* has a single assignment
operator. All top-level assignments are immutable. Local bindings can be mutated with `:=`.

```madlib
a = 1        // Integer
b = `cool`   // String
c = true     // Boolean
```

Mutation inside a function:
```madlib
counter = 0
counter := counter + 1
```


# Booleans

Booleans in *madlib* are lowercase, same as JavaScript:
```madlib
yes = true
no  = false
```


# Strings

Backticks are used for template strings, just like in JavaScript.
Double quotes are used for plain string literals.
Single quotes are used for `Char` literals (a `Char` is a 32-bit Unicode code point).

```madlib
template  = `Hello ${name}!`
literal   = "Hello world"
character = 'A'
```

String concatenation uses `++`:
```madlib
"Hello" ++ " " ++ "world"
```


# Numbers

```madlib
pi   = 3.1415  // Float
pies = 1       // Integer
```

There are also `Byte` (8-bit) and `Short` (32-bit) numeric types. Use a type annotation or
a literal suffix to select them:
```madlib
b = 255_b        // Byte
s = (1000 :: Short)
```


# Regular Expressions

Currently not natively supported by *madlib*. They are achievable with
[fencing](#the-fence).


# Objects / Records

Objects in *madlib* are called **Records**. Unlike JavaScript objects, record types are
structural and fully type-checked.

```madlib
language = { name: "Madlib", adjective: "cool" }
```

Field access:
```madlib
language.name  // "Madlib"
```

Creating an updated copy (records are immutable — spread creates a new record):
```madlib
updated = { ...language, adjective: "great" }
```

Pattern matching on a record:
```madlib
greet = where {
  { name: "Alice" } => "Hello Alice!"
  { name }          => `Hello ${name}!`
}
```

Records are **not** the same as ADT constructors. An ADT constructor wraps values:
```madlib
type User = User({ name :: String, age :: Integer })

user = User({ name: "Alice", age: 30 })

where(user) {
  User({ name: "The Boss" }) => "Hello Boss!"
  _                          => "Hello!"
}
```


# Expressions

*madlib* is expression-oriented. `if`/`else` and `where` are expressions, not statements,
and always produce a value.

```madlib
label = if (score > 90) { "A" } else { "B" }
```

The ternary operator also works:
```madlib
label = score > 90 ? "A" : "B"
```


# Functions

## Single expression per function

A function whose body is a single expression does not need braces:
```madlib
double = (x) => x * 2
```

## Auto-curry

All Madlib functions are automatically curried. Calling a multi-parameter function with
fewer arguments returns a new function waiting for the rest:
```madlib
add = (a, b) => a + b

inc    = add(1)       // Integer -> Integer
result = inc(5)       // 6
```

In JavaScript you would have to write `add = a => b => a + b` explicitly.

## Pattern matching

Instead of `switch` or chained `if`/`else`, use `where`:
```madlib
describe :: Integer -> String
describe = where {
  0 => "zero"
  1 => "one"
  n => `some number: ${show(n)}`
}
```

`where` works on any type and supports destructuring of ADTs, lists, tuples, and records.

## Implicit returns

In a block body the value of the last expression is **not** automatically returned — you
must write `return` explicitly:
```madlib
process = (x) => {
  doubled = x * 2
  return doubled + 1   // explicit return required
}
```

Single-expression functions (no braces) do return implicitly.


# Constructors

ADT constructors are functions. A constructor with `n` fields is a function of arity `n`:
```madlib
type Shape = Circle(Float) | Rectangle(Float, Float)

c = Circle(5.0)
r = Rectangle(3.0, 4.0)
```

Constructors can be partially applied just like regular functions:
```madlib
makeCircles = map(Circle)  // List Float -> List Shape
```


# Modules

A module is a `.mad` file. Exports are explicit:
```madlib
export myFunction = (x) => x + 1

export type Color = Red | Green | Blue
```

Importing:
```madlib
import { myFunction } from "./MyModule"
import IO from "IO"
import type { Color } from "./Color"
```

See [MODULES.md](MODULES.md) for the complete module and import syntax.
