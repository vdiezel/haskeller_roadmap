Deriving extensions

DeriveAnyClass

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/derive_any_class.html

Simplifies the automatic derivation of type class instances for user-defined
types, not only the native ones like Show, Eq, Ord etc.

Here is an example of a pretty printer class (the DefaultSignature
extensions allows automatic filling of the default implementation
of sPrp when a user not provide an implementation)

The compiler will generate an instance declaration with no explicitly
defined methods. useful for classes whose minimal set is empty.

> {-# LANGUAGE DefaultSignatures, DeriveAnyClass, DeriveGeneric, GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE DerivingStrategies, DerivingVia #-}
> {-# LANGUAGE StandaloneDeriving #-}

> import GHC.Generics
> import Numeric
>
> class SPretty a where
>   sPpr :: a -> String
>   default sPpr :: Show a => a -> String
>   sPpr = show

> data Foo = Foo deriving (Show, SPretty)

This line is equivalent to

> -- data Foo = Foo deriving Show
> -- instance SPretty Foo

DeriveGeneric

Allows automatically derive the "Generic" type class which grants
uniform representation of the data structure, which gives access to
manipulation and analysis using generic functions and libraries.

> data Person = Person
>   { name :: String
>   , age :: Int
>   } deriving (Show, Generic)

Generic comes with "to" and "from" methods that encode/decode a type "a"
to/from a generic representation, e.g. records to/from JSON.
Automatically derive JSON instances for Data.Aeson (JSON library):

> -- instance ToJSON Person
> -- instance FromJSON Person

GeneralizedNewtypeDeriving + DerivingStrategies

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_strategies.html

Allows to derive instances for a newtype based on the instances of the udnerlying type from
which it is deriving. Makes it easier to create newtype wrappers around existing types and reduce
boilerplate.

The compiler complains that GeneralizedNewtypeDeriving and DeriveAnyType are in conflict,
so it will default to DeriveAnyType, but that can be changed using DeriveStrategies.

without strategies the compiler will choose DeviceAnyClass here

> -- newtype MyInt = MyInt Int deriving (Eq, Ord, Show, Num)

with strategy chosing the "deriving newtype" strategy

> newtype MyInt = MyInt Int deriving newtype (Eq, Ord, Show, Num)

We could also use "anyclass" to use DeriveAnyClass, "stock" for "standard" instance for
a data type if possible (Eq, Ord, Functor etc.), and "via" (see below)

Deriving strategies also allows multiple deriving clauses per data
and therefore allowing different strategies per typeclass!

DerivingVia

(DerivingStrategies is implied by DerivingVia so does not need to be imported explictly)

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html#deriving-via

Allows deriving a class instance for a type by specifying another type that is
already an instance of that class using Data.Coerce.coerce.
It is a generalization of GeneralizedNewtypeDeriving.

> newtype Hex a = Hex a
>
> instance (Integral a, Show a) => Show (Hex a) where
>   show (Hex a) = "0x" ++ showHex a ""
>
> newtype Unicode = U Int
>   deriving Show
>     via (Hex Int)
>
> -- >>> euroSign
> -- 0x20ac
> euroSign :: Unicode
> euroSign = U 0x20ac

Generates this:

> -- instance Show Unicode where
> --   show :: Unicode -> String
> --   show = Data.Coerce.coerce
> --     @(Hex Int -> String)
> --     @(Unicode -> String)
> --     show

StandaloneDeriving

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/standalone_deriving.html?highlight=standalonederiving#extension-StandaloneDeriving

Simply allows standlone deriving declarations.

> data Foo1 a = Bar a | Baz String
> deriving instance Eq a => Eq (Foo1 a)

Differences from the "normal way of declaring things"
1. The standalone declaration can be in another module
2. In most cases, you must suplly an explicit context, e.g. here (Eq a) while
the attached declaration infers the context
3. The standalone declaration can be more specific than the data type (when used with FlexibleInstances)
... and more things I'm not able to grasp yet (GADTs)

> data Foo2 a = Bar2 a | Baz2 String
>
> deriving instance Eq a => Eq (Foo2 [a])
> deriving instance Eq a => Eq (Foo2 (Maybe a))

This limits the Eq class to [a] and Maybe a.

> main :: IO ()
> main = print "Hello Word"

