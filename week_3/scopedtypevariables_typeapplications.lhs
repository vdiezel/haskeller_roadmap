ScopedTypeVariables
https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/scoped_type_variables.html

the ScopedTypeVariables language pragma is used to control the scope of type variables in a function or expression.
It was introduced to address issues related to type variable scope and was included in the Haskell
language to provide more fine-grained control over type annotations within a function or expression.

The problem: type varaibles used in funciton signatures are usually scoped only to that function's signature.
This means that if you have a type variable in a function signature, it's not automatically in scope for the rest of the
function's body.

This default scoping behavior can sometimes be limiting, especially when you want to refer to a type variable
within the function body, or when you want to provide explicit type annotations for certain expressions inside the function.

> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeApplications #-}
>
> mkpair1 :: forall a b. a -> b -> (a,b)
> mkpair1 aa bb = (ida aa, bb)
>     where
>       ida :: a -> a -- This refers to a in the function's type signature
>       ida = id
>
> mkpair2 :: forall a b. a -> b -> (a,b)
> mkpair2 aa bb = (ida aa, bb)
>     where
>       ida :: b -> b -- Illegal, because refers to b in type signature
>       ida = id
>
> mkpair3 :: a -> b -> (a,b)
> mkpair3 aa bb = (ida aa, bb)
>     where
>       ida :: b -> b -- Legal, because b is now a free variable
>       ida = id

TypeApplications

Allows you to use type application snytax

> fn = show (read @Int "5")

> main :: IO ()
> main = do
>   print "Hello"
