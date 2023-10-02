Return type polymorphism a.k.a paramteric polymorphism

The ability of functions to operate on different types while keeping
type safety

In Haskell, done via type variables and type classes.

1. Type Variables

a is a type variable. Functions defined for the tree
will work for multiple types passed here. Of course when
we operate on a, we have to make sure a supports those operations

> data Tree a = Node a (Tree a) (Tree a)

2. Type constraints (via type classes)

We can put constraints on the passed types which will make sure
the passed types support given operation, e.g. different number types
support similar mathematical operations:

> mySum :: Num a => a -> a -> a
> mySum x y = x + y

3. Type classes

Like interfaces in other languages. Define a set of functions
that a type must implement. They enable adhoc polymorphism, a.k.a
function overloading.

Question: Can type constraints be used without type classes?
Not really - I think.

> main :: IO ()
> main = print "Hello Word"
