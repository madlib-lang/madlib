Modules allow to organize code and put together code that share functionalities or relates to common concepts. It is usually a good start to organize modules around types.


## Syntax

### Definition
A module consists of a madlib source file. A module can export types, functions or values.

Here is how expressions and types are exported:
```madlib
// inline type export
export type MyType = Constructor

// type export
type MyType = Constructor
// Note that the constructor is not exported in this case and needs to be exported separately
export type MyType

// inline expression export
export CONSTANT = "some constant"
export inc = (x) => x + 1

// expression export
CONSTANT = "some constant"
export CONSTANT
```

### Importing from a module
To import something from another module that is in the same project, `./` prefix must be used.

Given:
- /User.mad
- /Address.mad

To import Address in `User.mad` you would do:
```madlib
import { Address } from "./Address"

// now Address is available
```
Note that you can define import aliases and it is recommended to do all imports relative to the root of your source folder. For more information on import aliases refer to the documentation of packages, describing the format of `madlib.json` files.

To import something from an installed package you must simply use the name of the package. To import from the markdown parser one would do:
```madlib
import { parse } from "MarkdownParser"

parse("# some markdown")
```
Note that the same applies to standard library modules. For example to write a file to disk you'd import File like this:
```madlib
import File from "File"
import IO from "IO"
import { fulfill } from "Wish"

fulfill(
  pipe(inspect, IO.errLine),
  IO.log,
  File.write("./file.txt", "hello world")
)
```

### Import aliases
TBD
