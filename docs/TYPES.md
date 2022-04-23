# Type annotations
In Madlib there are two ways to give type annotation. You can annotate a function or assignment, or an expression.

## Function types
Function types are declared as follows:
```madlib
a -> b -> c
```
where `a` and `b` are parameter types and `c` is the type returned.

## Type variables
If you're familiar with ML languages it is exactly what you already know. Otherwise you can think of it as some kind of generic or type that can take any form. They are represented with lower cased words, usually even single lower cased characters.

**Important:** all type variables that are the same can only instantiate the same type. So in the function:
```madlib
identity :: a -> a
identity = (x) => x
```
the two `a` variables will always be the same concrete type when called. So `identity(3.3)` will always return a `Float` because it is called with a `Float`.

### Applying a type variable
The type `List` is not complete. It must be applied a type to form be complete. You must say what goes in that list. For example, a list of integers is defined as: `List Integer`. It is also possible to apply a type variable to it, so that we have a list of "whatever": `List a`. For example, here is how `concat` from the standard library is defined:
```madlib
concat :: List a -> List a -> List a
```

## Constraints
Analog to Haskell type classes, Madlib has interfaces (1). The standard library includes for example the `Inspect` interface, which as one method `inspect`. Now, if you want to have a function that calls it with a type variable as a parameter, say:
```madlib
pushAndLog = (item, list) => {
  IO.putLine(inspect(item))
  return [item, ...list]
}
```
The type of the above function is the following:
~~`a -> List a -> List a`~~

Well, almost! Because inspect is called on that item parameter of type `a`, there's a missing constraint. The fact that we call a method on that item implies that the type `a` must be a type that implements `Inspect`. Therefore the correct type is:
```madlib
Inspect a => a -> List a -> List a
```
A type annotation with constraints has the form:
`constraints => type` where constraints has the form `Interface typeVar` or `(Interface typeVar, OtherInterface otherVar)`.

Note that a type variable can have multiple constraints. Example:
```madlib
// for reference IO.log has type:
// Inspect a => a -> {}

sumAndLog :: (Number a, Inspect a) => a -> a -> a
sumAndLog = (a, b) => {
  result = a + b
  IO.log(result)
  return result
}
```

*1 - Have a look at `INTERFACES.md` for more information.*


## Function annotations
Here is the syntax to annotate a function:
```madlib
add :: Number a => a -> a -> a
add = (a, b) => a + b
```

Note that you can also annotate any assignment that way:
```madlib
SERVER_URL :: String
SERVER_URL = "https://myserver.tld"
```

## Expression annotations
Here is the syntax to annotate an expression:
```madlib
(expression :: type)
```
To avoid confusion, the parentheses are always necessary. Consider the otherwise confusing:
~~`1 + 3 :: Float`~~
Are we annotating `3` or `1 + 3`?

# Built-in types

## Integer
### with JS backend
Integers are represented as JS number.

### with LLVM backend
Integers are represented as 64 bit signed numbers.

### Examples
```madlib
1
-1
2384933
```

## Float
### with JS backend
Floats are represented as JS number.

### with LLVM backend
Floats are encoded as IEEE 754.

### Examples
```madlib
1.3
-1.3
138.75
```

## Byte
### with JS backend
Bytes are represented as JS numbers.

### with LLVM backend
Bytes are 8-bit unsigned numbers.

### Examples
```madlib
(1 :: Byte)
(255 :: Byte)
```

## Number interface
The `Number` `interface` defines operations that can be used with numbers. It is implemented for the following types:
- Integer
- Float
- Byte

Methods of Number:
- `+`
- `-`
- `*`
- `>`
- `<`
- `>=`
- `<=`

The default instance for Number is Integer. So if we're faced with an ambiguous instance, it resolves to Integer automatically except if a type annotation forces another type. Note that we had to do this above for the Byte examples as we'd otherwise would have gotten an Integer.

## String
### Examples
```madlib
"Hello World"
`Hello World`
```

## Char
### Examples
```madlib
'a'
'\n'
```

## Boolean
A value that can be `true` or `false`

### Examples
```madlib
true
false
```

## Unit
The unit type in Madlib is seen as the empty record.
The type and the value are represented by the same characters: `{}`.

### Examples
```madlib
{}
```

## List
### Examples
```madlib
numbers = [4, 5, 6]
[1, 2, 3, ...numbers] // [1, 2, 3, 4, 5, 6]
[true, false, false]
[{}, {}, {}, {}]
```

## Dictionary
Dictionary is a key/value collection. A given key can only be present once in the collection. A key can be any type that implements the `Comparable` interface.

### Examples
```madlib
{{ "key": "value" }}
```

## Set
Set is a collection similar to List, but all contained items are unique and ordered. Only values of types implementing Comparable can be part of a Set. There is no built-in syntax sugar for Set like there is for List or Dictionary. So you must use the constructor from the Set module of the standard library.

### Examples
```madlib
import Set from "Set"
Set.fromList([1, 2, 3])
```

## Tuples
A tuple is a n-dimension grouping of values. It is analog to vector and can in fact be used to describe vectors or similar thing, but also to group things. As a matter of a fact, the Dictionary constructor `fromList` accepts a List of 2-tuples `#[key, value]`.
The type is represented like this: `#[type1, type2, ...typeN]`.

### Examples
Tuple syntax is as follows:
```madlib
#[1, 2, 3]
#["a string", true, {}, {{ "key": "value" }}]
```

## ByteArray
A ByteArray is an array of Byte. It can be used to represent any byte sequence such as binary file content, image data, or pretty much anything as raw bytes. Like Set it does not have any special syntax but a few handy constructors and conversion functions such as: `fromList`, `toList`, `fromString`, `toString`.

### Examples
```madlib
import ByteArray from "ByteArray"

ByteArray.fromList([50, 100, 150])
ByteArray.fromString("Hello")
```

## Array
Array offers a similar functionality as List but the internals are very much different. It is represented by arrays in JS and as a contiguous set of items in memory with the llvm backend. It is more efficient to push items at the end of an array or concatenate two arrays than lists, but it is more efficient to push items in front of a list. Other than that most functions for lists have their array equivalent such as `map`, `filter`, `reduce`.

### Examples
```madlib
import Array from "Array"

Array.fromList([1, 2, 3])
```


## TO ADD:
- Records
- ADTs
- Aliases
