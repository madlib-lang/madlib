# FFI in Madlib


## JS Backend

### Functions
All madlib functions compiled to JS are curried and therefore have the form:
```js
let functionName = param1 => param2 => body
```

### ADTs
All ADTs take the same form once compiled. The type itself does not make it to the runtime;
only constructed values are left and have the form:
```js
{
  __constructor: "ConstructorName",
  __args: [arg1, arg2, ...] // the length matches the arity of the constructor
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

### Monomorphization
Madlib uses monomorphization to resolve all interface constraints at compile time.
There are **no interface dictionaries at runtime** and no dynamic dispatch. Functions that
are polymorphic in the source code are specialized into concrete versions for each type
they are used with, so from JavaScript's perspective every imported function has a fixed,
concrete signature.


## LLVM Backend

The LLVM backend compiles to native object files and links against the Madlib runtime
(`libruntime.a`). All parameters and return values in generated functions use `i8*` as
the uniform representation. See [ABI.md](ABI.md) for the complete type mapping.

### Calling Madlib functions from C

Madlib functions are compiled as top-level n-ary C functions:
```c
void* add(void* a, void* b);
```

All arguments and return values are `void*` (boxed). Primitive values are passed by
stuffing them into the pointer bits via `inttoptr` / `ptrtoint` — see the Boxing section
in [ABI.md](ABI.md).

When a function is partially applied a `PAP_t` struct is used instead:
```c
typedef struct PAP {
  void     *fn;              // pointer to the function
  int32_t   arity;           // total number of parameters
  int32_t   missingArgCount; // number of arguments still needed
  void     *env;             // struct holding already-applied arguments
} PAP_t;
```

### Extern declarations

Use `extern` to bind a Madlib name to a C function:
```madlib
length :: String -> Integer
export length = extern "madlib__string__length"
```

The string must match the C symbol name exactly. The C function must accept and return
`void*` (or compatible types such as `int64_t`, `double`, `char*`) according to the ABI
described in [ABI.md](ABI.md).

### Inline JS (JS backend only)

Use the fence syntax to embed raw JavaScript in JS-targeted builds:
```madlib
#iftarget js

toLower :: String -> String
export toLower = (s) => #- { return s.toLowerCase() } -#

#elseif llvm

toLower :: String -> String
export toLower = extern "madlib__string__toLower"

#endif
```

For single-line JS expressions use the inline fence:
```madlib
now :: {} -> Integer
export now = (_) => #- Date.now() -#
```

Content inside a fence is **opaque** to Madlib's type checker.
