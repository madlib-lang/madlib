# FFI in Madlib


## JS Backend

### Functions
All madlib functions compiled to JS are curried and take the manually curried form:
```js
let functionName = param1 => param2 => /* body */
```

Functions with interface constraints take additional curried parameters; for example, a function with this signature:
```madlib
insert :: Comparable k => k -> v -> Dictionary k v -> Dictionary k v
```
would compile to:
```js
let insert = comparableDict => k => v => dictionary => /* body */
```

### Interface dictionaries

Defined dictionaries take the form:

```js
let InterfaceName = {
  InstanceTypeName: {
    instanceMethodName: () => methodParam1 => methodParam2 => body
  }
}
```
In the above example, to call `insert` from JS (with Integer keys) would look like this:
```js
insert(Comparable.Integer)(3)("some value")(someDictionary)
```

### ADTs
Abstract Data Types take the same form once compiled. The type itself does not make it to the runtime (think abstract superclass) and only
instantiated values are considered and take the form:
```js
{
  __constructor: "ConstructorName",
  __args: [arg1, arg2, ...] // <- the length matches the arity of the constructor
}
```

For example, from this Madlib type:
```madlib
type Maybe a
  = Just(a)
  | Nothing
```

`Just(5)` is represented as:
```js
{
  __constructor: "Just",
  __args: [5]
}
```

And `Nothing` is represented as:
```js
{
  __constructor: "Nothing",
  __args: []
}
```


## LLVM Backend
