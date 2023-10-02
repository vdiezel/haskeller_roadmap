NonEmpty type

represents a non-empty list. It ensures that a list contains
at least one element making code semantically more precise.
It lives in the Data.List.NonEmpty which is part of the semigroups package.

> data NonEmpty a = a :| [a]

The ":|" is data constructor that constructs a NonEmpty list. It is
an example of encoding invariants at the type level. This can be done using GADTs,
Phantom types, type families, singleton and dependent haslell, type-level libraries.
I will learn more about them in the future.

> main :: IO ()
> main = print "Hello Word"
