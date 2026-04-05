# ABI - Application Binary Interface

This document describes how Madlib types are represented at runtime for both the LLVM and JavaScript backends. This is essential knowledge for writing FFI code and understanding how Madlib interoperates with C or JavaScript.


## Quick reference

| Madlib type | JavaScript | LLVM IR | C type |
|---|---|---|---|
| `Integer` | `number` | `i64` | `int64_t` |
| `Float` | `number` | `double` | `double` |
| `Byte` | `number` | `i8` | `uint8_t` |
| `Short` | `number` | `i32` | `int32_t` |
| `Char` | `string` (length 1) | `i32` | `int32_t` (Unicode code point) |
| `Boolean` | `boolean` | `i1` | `bool` |
| `String` | `string` | `i8*` | `char*` (null-terminated) |
| `Unit` (`{}`) | `{ __constructor: "Unit", __args: [] }` | `i8*` (null) | `void*` (NULL) |
| `List a` | `{ v, n }` linked list / `null` | `{ i8*, i8* }*` | `madlib__list__Node_t*` |
| `Array a` | `Array` | `{ i64, i64, i8** }*` | `madlib__array__Array_t*` |
| `ByteArray` | `Array` of numbers | `{ i64, i64, i8* }*` | `madlib__bytearray__ByteArray_t*` |
| `#[a, b, ...]` (Tuple) | `Array` | `{ fieldTypes... }*` | (anonymous struct pointer) |
| `{ f: a, ... }` (Record) | plain object | `{ fieldTypes... }*` (alphabetical) | (anonymous struct pointer) |
| Enum ADT | `{ __constructor, __args: [] }` | `i64` (tag index) | `int64_t` |
| Newtype ADT | `{ __constructor, __args: [v] }` | inner type (erased) | inner type |
| Single-constructor ADT | `{ __constructor, __args: [...] }` | `{ i8*... }*` (no tag) | (anonymous struct pointer) |
| Multi-constructor ADT | `{ __constructor, __args: [...] }` | `{ i64, i8*... }*` (tagged) | (anonymous tagged struct pointer) |
| Function (full application) | curried arrow `a => b => ...` | `i8* (i8*, ...)` | function pointer |
| Function (partial application) | curried arrow | `{ i8*, i32, i32, i8* }*` | `PAP_t*` |


## Primitive types

### Integer
#### LLVM backend
Represented as a 64-bit signed integer (`i64`).
#### JS backend
Represented as a JS `number`.

### Float
#### LLVM backend
Represented as an IEEE 754 double-precision floating-point (`double`).
#### JS backend
Represented as a JS `number`.

### Byte
#### LLVM backend
Represented as an 8-bit unsigned integer (`i8`).
#### JS backend
Represented as a JS `number`.

### Short
#### LLVM backend
Represented as a 32-bit signed integer (`i32`).
#### JS backend
Represented as a JS `number`.

### Char
#### LLVM backend
Represented as a 32-bit integer (`i32`) holding a Unicode code point.
#### JS backend
Represented as a JS `string` of length 1. Constructed via `String.fromCodePoint(codePoint)`.

### Boolean
#### LLVM backend
Represented as a 1-bit integer (`i1`).
#### JS backend
Represented as a JS `boolean` (`true` or `false`).

### String
#### LLVM backend
Represented as a null-terminated C string (`i8*` / `char*`).
#### JS backend
Represented as a JS `string`.

### Unit
#### LLVM backend
Represented as a null pointer (`i8*`).
#### JS backend
Represented as `{ __constructor: "Unit", __args: [] }`.


## Composite types

### List
#### LLVM backend
A singly-linked list. Each node is a struct pointer:
```c
typedef struct madlib__list__Node {
  void *value;
  struct madlib__list__Node *next;
} madlib__list__Node_t;
```
In LLVM IR: `{ i8*, i8* }*`. An empty list is a null pointer.

#### JS backend
A singly-linked list using plain objects:
```js
{
  v: value,
  n: nextNode // null for the last element
}
```
An empty list is `null`.


### Array
#### LLVM backend
A dynamically-sized contiguous array:
```c
typedef struct madlib__array__Array {
  int64_t length;
  int64_t capacity;
  void **items;
} madlib__array__Array_t;
```
In LLVM IR: `{ i64, i64, i8** }*`.

#### JS backend
Represented as a native JS `Array`.


### ByteArray
#### LLVM backend
A dynamically-sized contiguous byte buffer:
```c
typedef struct madlib__bytearray__ByteArray {
  int64_t length;
  int64_t capacity;
  unsigned char *bytes;
} madlib__bytearray__ByteArray_t;
```
In LLVM IR: `{ i64, i64, i8* }*`.

#### JS backend
Represented as a native JS `Array` of numbers.


### Tuple
#### LLVM backend
A struct pointer with one field per element. Primitive fields are stored unboxed at their
native type, other fields are stored as `i8*` (boxed). For example `#[Integer, String, Float]`
would be `{ i64, i8*, double }*`.

#### JS backend
Represented as a JS `Array`:
```js
#[1, "hello", 3.14]
// becomes:
[1, "hello", 3.14]
```


### Record
#### LLVM backend
A flat struct pointer with fields ordered **alphabetically by field name**. Like tuples,
primitive fields are stored unboxed at their native type and other fields are stored as
`i8*`. For example `{ name: String, age: Integer }` would be `{ i64, i8* }*` because
`age` comes before `name` alphabetically.

#### JS backend
Represented as a plain JS object:
```js
{ name: "Alice", age: 30 }
// stays as:
{ name: "Alice", age: 30 }
```


## Algebraic Data Types (ADTs)

ADTs are represented differently depending on the shape of their constructors. The compiler
optimizes the representation based on constructor count and arity.

### Enum ADTs (all constructors have zero fields)
#### LLVM backend
Represented as an `i64` tag value. Each constructor is assigned an integer index.
```madlib
type Color = Red | Green | Blue
// Red   -> 0
// Green -> 1
// Blue  -> 2
```

#### JS backend
```js
// Red:
{ __constructor: "Red", __args: [] }
```

### Single-constructor, single-field ADTs (newtypes)
#### LLVM backend
The wrapper is erased at runtime. The value is represented as the inner type itself (boxed as `i8*`).
```madlib
type Wrapper = Wrapper(String)
// Wrapper("hello") is just the string "hello" at runtime
```

#### JS backend
```js
// Wrapper("hello"):
{ __constructor: "Wrapper", __args: ["hello"] }
```

### Single-constructor, multi-field ADTs
#### LLVM backend
A struct pointer with one `i8*` field per constructor argument. There is no tag field since
there is only one constructor.
```madlib
type Point = Point(Float, Float)
// Point(1.0, 2.0) -> { i8*, i8* }*
//                      field 0: 1.0 (boxed)
//                      field 1: 2.0 (boxed)
```

#### JS backend
```js
// Point(1.0, 2.0):
{ __constructor: "Point", __args: [1.0, 2.0] }
```

### Multi-constructor ADTs
#### LLVM backend
A tagged struct pointer. The first field is an `i64` tag identifying the constructor, followed
by `i8*` fields for each argument (padded to the maximum arity across all constructors).
```madlib
type Maybe a = Just(a) | Nothing
// Just(5)  -> { i64, i8* }*  with tag=0, field 1 = 5 (boxed)
// Nothing  -> { i64, i8* }*  with tag=1
```

#### JS backend
All ADT constructors follow the same format:
```js
// Just(5):
{ __constructor: "Just", __args: [5] }

// Nothing:
{ __constructor: "Nothing", __args: [] }
```

### Dictionary
Dictionary is implemented as a red-black tree ADT with constructors `DictRBEmpty` and
`DictRBNode(color, key, value, left, right)`. It follows the standard multi-constructor
ADT representation in both backends.

### Set
Set is implemented as a red-black tree ADT with constructors `SetRBEmpty` and
`SetRBNode(color, value, left, right)`. It follows the standard multi-constructor ADT
representation in both backends.


## Functions

### LLVM backend
Functions are compiled as top-level **uncurried** n-ary definitions:
```llvm
define i8* @add(i8* %a, i8* %b) {
  ; body
}
```
All parameters and the return type are `i8*` (boxed).

When a function is partially applied, a PAP (Partially Applied function) structure is created:
```c
typedef struct PAP {
  void *fn;              // pointer to the function
  int32_t arity;         // total number of parameters
  int32_t missingArgCount; // number of arguments still needed
  void *env;             // pointer to a struct holding already-applied arguments
} PAP_t;
```
In LLVM IR: `{ i8*, i32, i32, i8* }*`.

The environment struct holds previously applied arguments as `void*` fields:
```c
typedef struct PAPEnv_2 {
  void *arg0;
  void *arg1;
} PAPEnv_2_t;
```

### JS backend
Functions are compiled as **curried** arrow functions:
```madlib
add = (a, b) => a + b
```
becomes:
```js
let add = a => b => a + b
```


## Monomorphization

Madlib uses monomorphization to resolve all polymorphism and interface constraints at compile
time. This means:

- There are **no interface dictionaries** at runtime. No dynamic dispatch occurs.
- Polymorphic functions are specialized into concrete versions for each type they are used
  with. For example, `map` used with `List Integer` and `map` used with `List String` become
  two separate functions in the compiled output.
- Interface method calls (e.g., `show`, `==`, `+`) are resolved statically to the concrete
  implementation for the specific type.

This applies to both the LLVM and JavaScript backends.


## Boxing and Unboxing (LLVM backend)

In the LLVM backend, values are boxed as `i8*` when stored in generic containers (e.g., list
nodes, ADT fields, array items) since these structures use uniform `i8*` slots. The
conversion works as follows:

| Type | Boxing (value -> i8*) | Unboxing (i8* -> value) |
|---|---|---|
| Integer | `inttoptr i64 -> i8*` | `ptrtoint i8* -> i64` |
| Float | `bitcast double -> i64`, then `inttoptr i64 -> i8*` | `ptrtoint i8* -> i64`, then `bitcast i64 -> double` |
| Byte | `inttoptr i8 -> i8*` | `ptrtoint i8* -> i8` |
| Short | `inttoptr i32 -> i8*` | `ptrtoint i8* -> i32` |
| Char | `inttoptr i32 -> i8*` | `ptrtoint i8* -> i32` |
| Boolean | `inttoptr i1 -> i8*` | `ptrtoint i8* -> i1` |
| String, List, ADT, etc. | `bitcast T* -> i8*` | `bitcast i8* -> T*` |

Tuples and records optimize this by storing primitive fields at their native types directly
in the struct, avoiding the boxing round-trip.


## Memory management (LLVM backend)

The LLVM backend uses the Boehm garbage collector for automatic memory management.

- **GC_malloc**: Used for allocating values that contain pointers (lists, ADTs, records,
  arrays, tuples, strings, closures).
- **GC_malloc_atomic**: Used for allocating values that do not contain pointers (raw byte
  buffers, etc.).

Primitive values (Integer, Float, Byte, Short, Char, Boolean, Unit) are not heap-allocated.
They are passed by value and boxed via `inttoptr` when needed for polymorphic contexts.
