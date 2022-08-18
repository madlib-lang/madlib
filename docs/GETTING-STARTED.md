# Introduction to Madlib

*Madlib* is a functional language which is a combination of [JavaScript](https://www.javascript.com) and [Haskell](https://www.haskell.org/). It is similar in nature to [PureScript](https://www.purescript.org/), [ReasonML](https://reasonml.github.io) and [Elm](https://elm-lang.org/).

It compiles to both Javascript and native binary executables. It is primarily designed for the web and can be used for client and server applications as well as scripting.

Its main goal is to enable users to write reliable code that can be easily tested and robust in a playful way. The playfulness and succint syntax is achieved via tacit ( aka pointfree ) notation and the `pipe` utility to compose functions. Here is a small example that displays some user files after reading them from disk, all in parallel:

```madlib
import type { Wish } from "Wish"

import Wish from "Wish"
import List from "List"
import File from "File"
import IO from "IO"

type User = User(String)

getInfoFilePath :: User -> String
getInfoFilePath = (user) => where(user) {
  User(path) =>
    path
}

readUserInfos :: List User -> Wish {} (List String)
readUserInfos = (users) => pipe(
  map(pipe(getInfoFilePath, File.read)),
  Wish.parallel,
  Wish.mapRej(() => ({}))
)(users)

Wish.fulfill(
  () => IO.errLine("Info files could not be read"),
  IO.log,
  readUserInfos([User("./users/1.csv"), User("./users/2.csv")])
)
```
