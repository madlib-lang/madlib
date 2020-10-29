![build](https://github.com/open-sorcerers/madlib/workflows/build/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/open-sorcerers/madlib/badge.svg?branch=master)](https://coveralls.io/github/open-sorcerers/madlib?branch=master)

# madlib

## build

`stack build`

## run

`stack run < fixtures/example.mad`
`cat fixtures/example.mad | stack run`

## hello world

```javascript
import IO from "IO"

IO.log("Hello World !")
```
