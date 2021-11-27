# Closures



%closure = { fnPointer, env }*

to call it:

%fnPtr = gep %closure, 0, 0
%fn    = load %fnPtr
%env   = gep %closure, 0, 1

%arg = ...

%returned = call %fn(%env, %arg)




definition of fn:

define external ccc  i8* @fn( i8* %env, i8*  %param ) {
  %closureEnv     = bitcast i8* %env to { i8* }*
  %closuredArgPtr = gep %closureEnv, 0, 0
  %closuredArg    = load %closuredArgPtr
  ...
}



Pseudo code for add :: Number -> Number -> Number

```llvm
define add i8* @add( i8* %emptyEnv, i8* %a ) {
  %numberA       = double unbox %a
  %allocEnv      = call malloc (sizeOf { sizeOf %numberA })
  %env           = bitcast %allocEnv i8* to { i8* }*
  %ptrA          = gep %env, 0, 0
  store %ptrA %numberA
  %addClosure    = load i8* (i8*)* @add_cls

  %allocClosure  = call malloc (sizeOf { addClosure, %env })
  %closure       = bitcast %allocClosure i8* to { i8*, i8* }*
  %closureFnPtr  = gep %closure, 0, 0
  %closureEnvPtr = gep %closure, 0, 1

  store %closureFnPtr %addClosure
  store %closureEnvPtr %env
}
```

In shorter terms, the above is a ClosureDef doing: `a -> Closure("add_cls", [Var a])`.
Therefore we do:
  - unbox the parameter
  - construct the closure struct to be returned

```llvm
define add i8* @add_cls( i8* %env, i8* %b ) {
  %unboxedEnv    =  { i8* } unbox env
  %numberA       = double unbox %b
  %allocEnv      = call malloc (sizeOf { sizeOf %numberA })
  %env           = bitcast %allocEnv i8* to { i8* }*
  %ptrA          = gep %env, 0, 0
  store %ptrA %numberA
  %addClosure    = load i8* (i8*)* @add_cls

  %allocClosure  = call malloc (sizeOf { addClosure, %env })
  %closure       = bitcast %allocClosure i8* to { i8*, i8* }*
  %closureFnPtr  = gep %closure, 0, 0
  %closureEnvPtr = gep %closure, 0, 1

  store %closureFnPtr %addClosure
  store %closureEnvPtr %env
}
```




ClosureDef "closure" [Var a] b body
closure = (env, b) => {
  insert a env[0] symbolTable
  
}
