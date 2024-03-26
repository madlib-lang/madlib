Interfaces are a tool to solve problems generically.

Let's take a concrete example; how can we implement an introspection function for a tuple (without interfaces)?
The function should take a binary tuple as input, and return its String representation:

```madlib
showList :: #[a, b] -> String
showList = (tuple) => where(tuple) {
  #[a, b] =>
    `#[${showA(a)}, ${showB(b)}]`
}
```


How could define introspection for individual values (`showA` and `showB`)?

The issue is that these aren't definite types and could be any type. Currently Madlib does not provide any sort of runtime reflection, however, interfaces can avail us here!

Let's see how to define the interface for our introspection problem:

```madlib
interface Show a {
  show :: a -> String
}
```
So with this definition, we're saying "if `Show` is implemented for a type `a`, we can call the method `show` with values of that type and get back a string."

Now we can use constraints within type annotations, to frame that a type variable implements a given interface.

The fat arrow is used to syntactically represent this constraint:
```madlib
Comparable a => a -> a -> Boolean
```
The type annotation above tells us that the function must be called with values of a type that implement the interface Comparable, otherwise we'll get a compilation error telling us that an `[instance] for Comparable was not found` for this type.

Coming back to our initial problem, we'd like to implement Show for the type `#[a, b]`. To do this, we can also constrain types in the beginning of an instance declaration:

```madlib
instance (Show a, Show b) => Show #[a, b] {
  show = (tuple) => where(tuple) {
    #[a, b] =>
      `#[${show(a)}, ${show(b)}]`
    }
}
```

Now we need not care about how `show` will be constrained for the types contained in the tuple. The `show` method will be dispatched to the right implementation, based on the concrete types the method is called with &mdash; So, `show(#[1, true])` would respectively call the method from `Show Integer` and `Show Boolean`.

