Modules allow you to organize and combine code which share functionality or commonality. It is usually a good idea to organize modules around types.


## Syntax

### Definition
A module consists of a madlib source file. A module can export types, functions or values.

This example frames the various ways that expressions and types can be exported:
```madlib
// inline type export
export type MyType = Constructor

type MyType = Constructor
// post-definition type export
export type MyType
// Note that the constructor is not exported in this case and needs to be exported separately

// inline expression export
export CONSTANT = "some constant"
export inc = (x) => x + 1

// post-expression export
CONSTANT = "some constant"
export CONSTANT
```

### Importing from a module
To import something from another module within the same project, a `./` prefix should be used. (See [import aliases](#tbd-where-does-this-live) for elegant alternatives)

Given two files:
- User.mad
- Address.mad

To import Address in `User.mad` you can do:
```madlib
import { Address } from "./Address"
// ^-- now Address is available
```

*Note*: You can define [import aliases](#tbd-where-does-this-live) and we recommend defining imports relative to the root of your source folder.

To import something from an [installed package](#how-to-install-a-package) use the name of the package in the format:
```madlib
import { memberValue } from "module-name"
```

For example, to import from the markdown parser:
```madlib
import { parse } from "MarkdownParser"

parse("# some markdown")
```

*Note*: This applies to standard library modules. For example, in order to write a file to disk you can `import File` like this:
```madlib
import File from "File"
import IO from "IO"
import { fulfill } from "Wish"

fulfill(
  pipe(show, IO.errLine),
  IO.log,
  File.write("./file.txt", "hello world")
)
```

### Import aliases
TBD
