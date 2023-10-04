Semigroup

A mathematical construct with

1. A non-empty set
2. a binary operation defined on the set, f:: a -> a -> a
3. Associativity of that operation

Binary operatorion indicated as "<>"

Example:

> import Data.Semigroup
> import Data.Monoid

> instance Semigroup Int where
>   (<>) = (+)

> result :: Int
> result = 2 <> 3

They are fundamental conecepts in abstract algebra and allow combining elements
in structured and associative manner in functional programming.

Monoid

a mathematical construct that builds on top of
a semigroup. The only difference is that the set contains
an identity element, also called neutral element (like 0 in addition
or 1 in product), denoted as "e" or "mempty"

> -- Define a Monoid instance for integers using the Sum newtype
> newtype SumInt = SumInt { getSumInt :: Int }
>
> instance Monoid SumInt where
>   mempty = SumInt 0
>   mappend (SumInt x) (SumInt y) = SumInt (x + y)

Often used to represent various types of collection or computations
that can be combined, e.g. concatenation of lists with the empty list
as its idenity and numeric types for addition and product.

Typical semigroups are:
  Min,
  Max,
  First: Maybe semigroup, simply returns the first value, even when it is Nothing
  Last: Maybe semigroup, simply returns the last value, even when it is Nothing,
  All: Boolean monoid under conjunction (&&)
  Any: Boolean monoid under disjunction (||)

Typical monoids are:
  Sum
  Product
  First: Maybe monoid returning the leftmost non-nothing value
  Last: Maybe monoid returning the rightmost non-nothing value

> main :: IO ()
> main = do
>   print "Hello"
