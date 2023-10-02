IORef, State and ST monad

IORef (from Data.IORef)

Provides a mutable (!) reference in the IO monad.
For mutable state shared and modified acriss different parts
of IO.

> import Data.IORef

> import Control.Monad
> import Control.Monad.State

> import Control.Monad.ST
> import Data.STRef
>
> main :: IO ()
> main = do
>     ref <- newIORef 0
>     replicateM_ 5 $ do
>         val <- readIORef ref
>         writeIORef ref (val + 1)
>         print val

State Monad

Pure monad for threading state through a sequence
of computations. Encapsulates state computations in a purely functional way.
For managing state within
purely functional context.
No side effects.
State changes are modelled as a sequence of transformations.
Convenient to prevent explicit passing of the state variable.

> -- Define a stateful computation that increments a counter
> incrementCounter :: State Int ()
> incrementCounter = do
>     state <- get
>     put (state + 1)
>
> -- Run the computation with an initial state of 0
> main1 :: IO ()
> main1 = do
>     let ((), newState) = runState incrementCounter 0
>     print newState  -- Outputs: 1

ST Monad (Strict State Threads)

Controlled and safe way of working with mutable state in a purely functional
context.
Allows true in-place mutation of data without violating referential
transparency.
Uses "rank-2 polymorphism" to ensure that mutable state does not leak
Often used in performance critical tasks.

> -- Create and use an STRef (mutable reference)
> incrementCounter2 :: ST s Int
> incrementCounter2 = do
>     ref <- newSTRef 0
>     writeSTRef ref 1
>     readSTRef ref
>
> main2 :: IO ()
> main2 = do
>     let result = runST incrementCounter2
>     print result  -- Outputs: 1
