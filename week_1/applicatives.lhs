Applicatives and why use them over Monads

An applicative is a type class that is build on top of functors.
It is somewhat in the middle between functors and monads.
It allows working on values within a context while preserving it,
just like functors.

The type class asks for two things:

1. A "pure" function that wraps it within the applicative context

2. the "<*>" operator, called "ap"

> class Functor f => Applicative f where
>   pure :: a -> f a
>   (<*>) :: f (a -> b) -> f a -> f b

So the difference to a normal functors "fmap" is that it takes a function within
a context. This makes it useful when you want to apply a function that
takes multiple arguments to values that are wrapped in an applicative context
while a functor is limited to unary functions.

E.g. adding two Maybes:

> result = Just (+2) Prelude.<*> Just 3

> main :: IO ()
> main = print "Hello Word"

Why to use them over monads?

They are simpler. If you have computations that are not sequential
, i.e. a step does not depend on the result of the previous step, they
are the better choice.

They can be parallelized easier.
Applicatives can be optimized more aggressively by the compiler.

Once you need higher control over the control flow, you have to use a
monad, e.g. sequential execution.
