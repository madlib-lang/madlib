# Migrating to *madlib* from JavaScript
## Common Pitfalls & Delights

*madlib* is a constructed language which is intended to make the syntax and semantics of JavaScript more palatable and expressive, while restricting certain constructs.

1. [Types](#types)
2. [The Fence](#the-fence)
3. [Literals](#literals)
- [Booleans](#booleans)
- [Strings](#strings)
- [Numbers](#numbers)
- Regular Expressions - not currently supported without fencing
4. [Object Literals](#objects)
5. [Expressions](#expressions)
6. [Functions](#functions)
- Single Expression per Function
- Autocurry
- Pattern-Matching
- Implicit Returns
7. [Constructors](#constructors)
- Type Constructors
8. [Modules](#modules)

# Types

One of the essential utilities of *madlib* is its type inference system. Where possible you should use its types in order to provide correctness and allow for some powerful features which only work with types defined.

## Global Types

* `String` - for use with string values
* `Number` - for use with number values
* `Boolean` - for use with boolean values
* `List` - for use with array

## Defining your own types

# The Fence

In the case that you want to do something in *madlib* which is not yet expressible in the current native *madlib* syntax, simply wrap a valid JS expression with either the single-line `#- "JAVASCRIPT" -#` or a multiline:

```
#- {
"JAVASCRIPT"
} -#
```

There is a downside to doing this however, as content within the fence is opaque to *madlib*'s type inference.

# Literals

Much of *madlib*'s syntax is identical to JavaScript, but there are a few discrepancies.

# Variables

Instead of letting / forcing (depending on your perspective) you to choose the different semantics of `var` / `let` / `const`,  *madlib* simply has an assignment operator.

```
a = 1 // Number
b = `cool` // String
c = true // Boolean
```

# Booleans

Minor discrepancy, Booleans in *madlib* are capitalized.

```
yes = true
no = false
```

# Strings

Just like JavaScript.

```
iAmAString = `yes you are`
iAmDoublyStrung = "weird brag"
strictlyBetter = `obviously`
```

# Numbers

Just like JavaScript.

```
pi = 3.1415
pies = 1
```

# Regular Expressions

Currently these are not natively supported by *madlib*. They are achievable with [fencing](#the-fence).

# Objects

Objects in *madlib* are called Records.

They can be used just like object literals:
```
language = { name: "Madlib", adjective: "cool" }
```
They can also be used as constructor functions:
```
data = 
user = User({name: "Alice", age: 300, email: "john@methusela.org"})
where (user) {
  is User { name: "The Boss" }: "Hello Boss!"
  is _                        : "Not
}
```

