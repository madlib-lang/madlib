# Functions
Functions are compiled to top level n-ary definitions eg:

```llvm
define external ccc  i8* @add(i8*  %a, i8*  %b) {
  ; body
}
```

# Different scenarios

## known call
```madlib
add = (a, b) => a + b

add(2, 3)
```
In this example we are faced we a known call, this means that we know exactly what function
is being called but also all arguments are provided. In that case we just pull the function
from the symbol table and make the call directly.

## partial application
```madlib
add = (a, b) => a + b

inc = add(1)
```

In this example we are faced we a partial application, not all args are provided. We must
therefore create a PAP. A PAP is a structure holding partially applied arguments and
metadata about the arity of the function, how many args must still be applied. It is defined as:

```llvm
{ i8*, i32, i32, i8* }*
{ function ptr, arity, amount of args still to be applied, env ptr }
```

So the above inc would create the following structure:
```llvm
{ add*, 2, 1, { 1 }}
{ it points to the function add, arity of add is 2, it still needs 1 arg, { the applied arg is 1 }}
```

## calling a PAP

