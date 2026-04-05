## Literals

### Integer
Integer literals are whole numbers, possibly prefixed with `-`.

#### Example
```madlib
1
-1732
```

### Byte
Byte literals are whole numbers between 0 and 255. The type must be forced explicitly, either
with a type annotation or with the `_b` numeric suffix.

#### Example
```madlib
(1 :: Byte)
(255 :: Byte)
10_b + 20_b
```
Note that without the annotation or suffix the literal resolves to `Integer` due to the
default `Number` instance resolution.

### Short
Short literals are 32-bit signed integers. Use the `_s` suffix or a type annotation.

#### Example
```madlib
(3 :: Short)
3_s
range(1_s, 5_s) // [1_s, 2_s, 3_s, 4_s, 5_s]
```

### Float
Float literals are numbers with a decimal part, possibly prefixed with `-`. The `_f` suffix
can also be used.

#### Example
```madlib
1.0
-1732.05
-1_f
```

### String
Strings are characters within double quotes, or backticks for string templates.

#### Example
```madlib
"hello world"
`https://domain.tld/users/${userId}`
```

### Character
Character literals are single characters within single quotes.

#### Example
```madlib
'a'
'\n'
```

### Boolean
Can be either `true` or `false`.

#### Example
```madlib
true
false
```

### Unit
Only has one possible value: `{}`. It can be seen as the empty record and is used to
represent the absence of a meaningful return value.

```madlib
doSomething :: String -> {}
doSomething = (s) => {
  IO.log(s)
  return {}
}
```


## Lambdas
In Madlib a function is a lambda that can be assigned, passed around, or used inline.

#### Example
```madlib
(a, b) => a * b
```

Multi-line lambdas use a block body. The last expression in the block is returned if a
`return` statement is not used.
```madlib
process = (x) => {
  doubled = x * 2
  return doubled + 1
}
```


## Function application
Functions are called with parentheses. Madlib functions are auto-curried — partial
application produces a new function.

```madlib
add(1, 2)       // full application → 3
inc = add(1)    // partial application → a function Integer -> Integer
inc(5)          // → 6
```


## Binary operators

| Operator | Types | Description |
|---|---|---|
| `+`, `-`, `*` | `Number a => a` | Arithmetic |
| `/` | `Float` | Division (Float only) |
| `%` | `Integer` | Modulo |
| `>`, `<`, `>=`, `<=` | `Comparable a => a` | Comparison |
| `==`, `!=` | `Eq a => a` | Equality |
| `&&` | `Boolean` | Logical AND |
| `\|\|` | `Boolean` | Logical OR |
| `++` | `String` | String concatenation |
| `<>` | `List a` | List concatenation |

```madlib
1 + 2         // 3
"hello" ++ " world"  // "hello world"
[1, 2] <> [3, 4]     // [1, 2, 3, 4]
3 > 2 && true        // true
```


## Unary operators

| Operator | Type | Description |
|---|---|---|
| `!` | `Boolean` | Logical NOT |

```madlib
!true  // false
```


## Ternary expression
The ternary operator is `condition ? thenExpr : elseExpr`.

```madlib
x > 0 ? "positive" : "non-positive"

// Can be multi-line:
isValid
  ? processValid(value)
  : handleInvalid(value)
```


## If / else
`if`/`else` is an expression — it always evaluates to a value.

```madlib
if (x > 0) {
  "positive"
} else if (x == 0) {
  "zero"
} else {
  "negative"
}
```


## Where (pattern matching)
`where` is the primary way to inspect and destructure values.

### Basic form
```madlib
where(expression) {
  pattern1 =>
    result1

  pattern2 =>
    result2
}
```

### Pointfree form
When the scrutinee is the last argument a `where` can omit it and act as a lambda:
```madlib
isJust :: Maybe a -> Boolean
isJust = where {
  Just(_) => true
  Nothing => false
}
```

### Pattern types

**Wildcard** — matches anything, does not bind:
```madlib
where(x) {
  _ => "anything"
}
```

**Variable** — matches anything and binds it:
```madlib
where(x) {
  n => n + 1
}
```

**Literal** — matches an exact value:
```madlib
where(n) {
  0 => "zero"
  1 => "one"
  _ => "other"
}
```

**Constructor** — matches an ADT constructor and binds its fields:
```madlib
where(maybe) {
  Just(x) => x
  Nothing => 0
}
```

**List** — matches a list head and tail:
```madlib
where(list) {
  [first, second, ...rest] => first + second
  [only]                   => only
  []                       => 0
}
```

**Tuple** — matches each element:
```madlib
where(pair) {
  #[a, b] => a + b
}
```

**Record** — matches named fields:
```madlib
where(user) {
  { name: "Alice" } => "Hello Alice!"
  { name }          => `Hello ${name}!`
}
```

**Spread in constructor** — matches remaining list elements:
```madlib
where(list) {
  [head, ...tail] => head
}
```

**Nested patterns** — patterns can be nested arbitrarily:
```madlib
where(value) {
  Just(#[x, y]) => x + y
  _             => 0
}
```


## Do notation
`do` blocks sequence monadic computations. Use `<-` to bind the result of a monadic action
and `return` to lift a pure value.

```madlib
jsonString :: Parser Value
jsonString = do {
  _  <- char('"')
  cs <- many(stringCharacter)
  _  <- symbol(`"`)
  return pipe(String.fromList, JsonString, of)(cs)
}
```

```madlib
do {
  a <- parserA
  b <- parserB
  c <- parserC
  return of(combine(a, b, c))
}
```


## Pipe
The `pipe` function composes a list of functions left-to-right and applies them to an
initial value. It is the idiomatic way to chain transformations in Madlib.

```madlib
pipe(
  map(double),
  filter(isEven),
  List.reverse,
)(numbers)
```

This is equivalent to `List.reverse(filter(isEven, map(double, numbers)))`.


## Record expressions

### Creation
```madlib
{ name: "Alice", age: 30 }
```

### Field access
```madlib
user.name
```

### Spread / update
```madlib
{ ...user, age: 31 }               // update one field
{ ...defaults, ...overrides }      // merge two records
```


## List expressions

### Creation
```madlib
[1, 2, 3]
[]
```

### Spread
```madlib
[1, 2, ...moreNumbers]
```


## Tuple expressions
```madlib
#[1, "hello", true]
#["x", "y", "z"]
```


## String interpolation
Backtick strings support interpolation with `${}`:
```madlib
name = "World"
`Hello ${name}!`
`The answer is ${show(40 + 2)}`
```


## Type annotations
Any expression can be annotated with `(expression :: Type)`:
```madlib
(42 :: Integer)
(3.14 :: Float)
([] :: List String)
```


## Extern
Binds a Madlib name to a foreign (C or JS) symbol. See [FFI.md](FFI.md) for details.

```madlib
length :: String -> Integer
export length = extern "madlib__string__length"
```


## Inline JS (JS backend only)
Embeds a raw JavaScript expression. The result is opaque to Madlib's type checker.

Single-line:
```madlib
now :: {} -> Integer
export now = (_) => #- Date.now() -#
```

Multi-line:
```madlib
toLower :: String -> String
export toLower = (s) => #-
  s.toLowerCase()
-#
```


## Conditional compilation
Use `#iftarget` / `#elseif` / `#endif` to provide different implementations for different
compile targets:
```madlib
#iftarget js

toLower :: String -> String
export toLower = (s) => #- { return s.toLowerCase() } -#

#elseif llvm

toLower :: String -> String
export toLower = extern "madlib__string__toLower"

#endif
```


## Mutable variables
Inside a function body a `:=` assignment mutates an existing binding:
```madlib
counter = 0
loop = () => {
  counter := counter + 1
  return counter
}
```

Note that `:=` only works within a local scope; top-level definitions are always immutable.
