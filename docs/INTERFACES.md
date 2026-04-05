Interfaces are a tool to extend genericity possibilities. Let's take a concrete example to
illustrate it. Let's see how we could implement a `show` function for a tuple without
interfaces. The function should take a 2-tuple as input and return its String representation:
```madlib
inspectList :: #[a, b] -> String
inspectList = (tuple) => where(tuple) {
  #[a, b] =>
    `#[${inspectA(a)}, ${inspectB(b)}]`
}
```
Now, what we would like to do is to define `inspectA` and `inspectB`. The issue is that these
aren't defined types and could be any type. Madlib does not provide any sort of runtime
reflection. Luckily, interfaces come to the rescue! Let's see how to define the interface for
our show problem:
```madlib
interface Show a {
  show :: a -> String
}
```
So with this we say that if `Show` is implemented for a type `a`, we can call the method
`show` with values of that type, which takes that type and returns a String.

Now we can use constraints within type annotations to force a type variable to be a type that
implements an interface. The fat arrow is used for that purpose:
```madlib
Comparable a => a -> a -> Boolean
```
Here the type annotation tells us that the function must be called with values of a type that
implements the interface `Comparable`, or else we'd get a compilation error telling us that an
instance for `Comparable` was not found for that type.

Now back to our initial problem: we'd like to implement `Show` for the type `#[a, b]`. To do
this we can also constrain types in the head of an instance declaration:
```madlib
instance (Show a, Show b) => Show #[a, b] {
  show = (tuple) => where(tuple) {
    #[a, b] =>
      `#[${show(a)}, ${show(b)}]`
    }
}
```
Now we can finally do it, and we need not care about how `show` will be done for the types
contained in the tuple.

### How dispatch works

Madlib uses **monomorphization**: all interface constraints are resolved entirely at compile
time. When you write `show(#[1, true])`, the compiler statically determines that the tuple
contains an `Integer` and a `Boolean` and generates a specialized version of `show` that
calls `Show Integer`'s implementation for `1` and `Show Boolean`'s implementation for `true`.
There is no runtime dictionary lookup or dynamic dispatch; the correct implementation is
hardwired into the compiled output.

### Defining an interface

```madlib
interface Show a {
  show :: a -> String
}
```

An interface can have a superclass constraint:
```madlib
interface Eq a => Ord a {
  compare :: a -> a -> Comparison
}
```

### Implementing an interface

```madlib
instance Show Integer {
  show = (n) => #- { return n.toString() } -#
}
```

With constraints on the type variables:
```madlib
instance Show a => Show (List a) {
  show = (list) => where(list) {
    [] =>
      "[]"

    _ =>
      `[${String.join(", ", map(show, list))}]`
  }
}
```
