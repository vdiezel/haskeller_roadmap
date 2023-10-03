Foldable

Defines data structures that can be folded or reduced into a single value.
The interface defines following functions:

foldr: Takes a binary function, an initial accumulator value
and applies the function to all values and accumulating the result, starting
from the right-most element

foldl: like foldr, but starting from the left-most element

foldMap: map each element to a monoid, then combine the monoid values.
It takes the mapping function

toList: converts a foldable structure to a list

null: checks if a foldable structure is empty

length: number of elements in a foldable structure

Here is the implementation for lists

> import Data.Foldable
> import Data.Monoid
> import Data.Traversable
> import Control.Applicative
>
> instance Foldable [] where
>     -- foldr applies the binary function from right to left
>     foldr :: (a -> b -> b) -> b -> [a] -> b
>     foldr _ acc []     = acc
>     foldr f acc (x:xs) = f x (foldr f acc xs)
>
>     -- foldMap maps each element to a monoid and combines the results
>     foldMap :: Monoid m => (a -> m) -> [a] -> m
>     foldMap _ []     = mempty
>     foldMap f (x:xs) = f x `mappend` foldMap f xs

I'm not sure yet how exactly Monoids work in Haskell, so "mappend" is a riddle.
I think I will learn that down the road. Here is an example code:

> printHelloWorld :: IO ()
> printHelloWorld = do
>     let stringList = ["Hello", ", ", "World", "!"]
>
>     -- Concatenate the strings in the list using foldMap;
>     -- but according to the linter I could just use "fold"
>     let concatenatedString = foldMap id stringList
>
>     putStrLn $ "Concatenated String: " ++ concatenatedString

Traversable

Higher order type class that generalizes the concept of traversing a data structure while applying an
effect (usually a functor) to each element.

Abstraction over common patterns of iteration and transformation, making it easier to work with
container like structures. Related to Functors and Foldable.

> -- class (Functor t, Foldable t) => Traversable t where
> --     traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
> --     sequenceA :: Applicative f => t (f a) -> f (t a)
> --     mapM :: Monad m => (a -> m b) -> t a -> m (t b)
> --     sequence :: Monad m => t (m a) -> m (t a)

Gives the traverse function:

> -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

Here we can traverse a list, doubling every element but also printing it at the same
time using the IO applicative. Nice for debugging I thing.

> main :: IO ()
> main = do
>     let myList = [1, 2, 3]
>
>     -- Traverse the list, doubling each element using IO as the applicative functor
>     doubledList <- traverse (\x -> putStrLn ("Doubling " ++ show x) *> pure (2 * x)) myList
>
>     putStrLn $ "Doubled List: " ++ show doubledList
