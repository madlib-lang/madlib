## Literals

### Integer
Integer literals are whole numbers, possibly prefixed with `-`.

#### Example
```madlib
1
-1732
```

### Byte
Integer literals are whole numbers between 0 and 255. 

#### Example
```madlib
(1 :: Byte)
(255 :: Byte)
(0 :: Byte)
```
Note: if used in a context where the type is not forced to be a byte it might resolve to an integer due to how the Number instance default resolution works.

### Float
Float literals are numbers with a decimal value, possibly prefixed with `-`.

#### Example
```madlib
1.0
-1732.05
```

### Character
Character literals are single characters within single quotes.

#### Example
```madlib
'a'
'\n'
```

### String
Strings are characters within double quotes, or backticks when using string templates.

#### Example
```madlib
"hello world"
`https://domain.tld/users/${userId}`
```

#### Boolean
Either true or false.

#### Example
```madlib
true
false
```

#### Unit
This has only one value possible: `{}` and is the empty record.

## Lambdas
In Madlib, a function is simply a lambda, that can be assigned, or directly passed around or used in-line.

#### Example
```madlib
(a, b) => a * b
```
