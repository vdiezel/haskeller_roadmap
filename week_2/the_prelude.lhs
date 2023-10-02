Prelude

The standard module that is automatically imported. One can create a custom prelude like this:

1. Creat a custom prelude module, e.g. CustomPrelude.hs
2. Define functions and Types

> module CustomPrelude
>     ( module Prelude
>     , myFunction
>     , MyType (..)
>     ) where
>
> import Prelude hiding (head, tail)  -- Hide standard Prelude functions
>
> -- Import specific functions/types from the standard Prelude
> import Prelude (Bool(..), Eq(..), Show(..))
>
> -- Define custom functions/types
> myFunction :: Int -> Int
> myFunction x = x * 2
> data MyType = MyConstructor1 | MyConstructor2 deriving (Eq, Show)

3. import the CustomPrelude everywhere

One can use the NoImplicitPrelude pragma to prevent the default Prelude to be used.
Also if you call your custom module Prelude in a file Prelude.hs, it will be
automatically implictly imported instead of the default Prelude.

