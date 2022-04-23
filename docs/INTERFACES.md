Interfaces are a tool to extend genericity possibilities. Let's take a concrete example to illustrate it. Let's see how we could implement an inspect function for a tuple without interfaces. The function should take a 2-tuple as input, and return its String representation:
```madlib
inspectList :: #[a, b] -> String
inspectList = (tuple) => where(tuple) {
  #[a, b] =>
    `#[${inspectA(a)}, ${inspectB(b)}]`
}
```
Now, what we would like to do, is to define inspectA and inspectB. The issue is that these aren't defined types and could be any type. Unfortunately Madlib does not provide any sort of reflection like Java that would allow us to figure out at runtime how the value is structured. Luckily, interfaces come to the rescue! Let's see how to define the interface for our inspect problem:
```madlib
interface Inspect a {
  inspect :: a -> String
}
```
So with this we say, that if Inspect is implemented for a type `a`, we can call the method inspect with values of that type, that take that type and return a string.

Now we can use constraints within type annotations, to force a type variable to be a type that implements an interface. The fat arrow is used for that purpose:
```madlib
Comparable a => a -> a -> Boolean
```
Here the type annotation tells us that the function must be called with values of a type that implement the interface Comparable or else we'd get a compilation error telling us that an instance for Comparable was not found for that type.

Now back to our initial problem, so we'd like to implement Inspect for the type `#[a, b]`. To do this, we can also constrain types in the head of an instance declaration:
```madlib
instance (Inspect a, Inspect b) => Inspect #[a, b] {
  inspect = (tuple) => where(tuple) {
    #[a, b] =>
      `#[${inspect(a)}, ${inspect(b)}]`
    }
}
```
Now we can finally do it, and we need not care about how inspect will be done for the types contained in the tuple. The inspect method will be dispatched to the right implementation, based on the concrete types the method is called with. So:
inspect(#[1, true]) would respectively call the method from `Inspect Integer` and `Inspect Boolean`.

