# FFI in Madlib


## JS Backend

### Functions
All madlib functions compiled to JS are curried and therefore have the form:
```js
let functionName = param1 => param2 => body
```

Functions with interface constraints take additional parameters, so for example a function with this signature:
```madlib
insert :: Comparable k => k -> v -> Dictionary k v -> Dictionary k v
```
would result in JS as:
```js
let insert = comparableDict => k => v => dictionary => body
```

### Interface dictionaries
All defined dictionaries take the form:
```js
let InterfaceName = {
  InstanceTypeName: {
    instanceMethodName: () => methodParam1 => methodParam2 => body
  }
}
```
Therefore in the above example, if one was to call insert from JS with Integer keys it should be called like this:
```js
insert(Comparable.Integer)(3)("some value")(someDictionary)
```

### ADTs
All ADTs take the same form once compiled, the type itself does not make it to the runtime and only
constructed values are left and have the form:
```js
{
  __constructor: "ConstructorName",
  __args: [arg1, arg2, ...] // <- the length matches the arity of the constructor
}
```
Therefore, from this Madlib type:
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
