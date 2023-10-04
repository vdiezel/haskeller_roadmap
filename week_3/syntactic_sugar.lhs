> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE NumericUnderscores #-}

LambdaCase
https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/lambda_case.html

allows

> -- \case { p1 -> e1; ...; pN -> eN }

which is the same as

> -- \freshName -> case freshName of { p1 -> e1; ...; pN -> eN }

Since GHC 9.4.1 you can also have multiple scrutinees

> -- \cases { p11 ... pM1 -> e1; ...; p1N ... pMN -> eN }

NumericUnderscores
https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/numeric_underscores.html

Allows adding one or multiple underscores within decimal, octal, hexadecimal, binary and float notations.

> -- decimal
> million    = 1_000_000
> billion    = 1_000_000_000
> lightspeed = 299_792_458
> version    = 8_04_1
> date       = 2017_12_31
>
> -- hexadecimal
> redMask = 0xff_00_00
> size1G   = 0x3fff_ffff
>
> -- binary
> bit8th   = 0b01_0000_0000
> packbits = 0b1_11_01_0000_0_111
> bigbits  = 0b1100_1011__1110_1111__0101_0011
>
> -- float
> pi       = 3.141_592_653_589_793
> faraday  = 96_485.332_89
> avogadro = 6.022_140_857e+23
>
> -- function
> isUnderMillion = (< 1_000_000)
>
> clip64M x
>     | x > 0x3ff_ffff = 0x3ff_ffff
>     | otherwise = x

> x0 = 1_000_000   -- valid
> x1 = 1__000000   -- valid
> x2 = 1000000_    -- invalid
> x3 = _1000000    -- invalid
>
> e0 = 0.0001      -- valid
> e1 = 0.000_1     -- valid
> e2 = 0_.0001     -- invalid
> e3 = _0.0001     -- invalid
> e4 = 0._0001     -- invalid
> e5 = 0.0001_     -- invalid
>
> f0 = 1e+23       -- valid
> f1 = 1_e+23      -- valid
> f2 = 1__e+23     -- valid
> f3 = 1e_+23      -- invalid
>
> g0 = 1e+23       -- valid
> g1 = 1e+_23      -- invalid
> g2 = 1e+23_      -- invalid
>
> h0 = 0xffff      -- valid
> h1 = 0xff_ff     -- valid
> h2 = 0x_ffff     -- valid
> h3 = 0x__ffff    -- valid
> h4 = _0xffff     -- invalid

MultiWayIf
https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/multiway_if.html

Very useful this one! Allows writing these:

> if | guard1 -> expr1
>    | ...
>    | guardN -> exprN

or even these:

> if | guard1 -> if | guard2 -> expr2
>                   | guard3 -> expr3
>    | guard4 -> expr4

and these

> if | parseNumbers settings
>    , Just (exponent, mantissa) <- decomposeNumber str
>    , let (integralPart, fractionPart) = parse mantissa
>    , integralPart >= 0 = ...
>    | otherwise = ...

TupleSections
https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/tuple_sections.html

Enables partially applied touples. I like this one.

> (, True)

instead of

> \x -> (x, True)

or

> (, "I", , , "Love", , 1337)

instead of

\a b c d -> (a, "I", b, c, "Love", d, 1337)

BlockArguments
https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/block_arguments.html

Allow do expressions, lambda expressions etc. to be directly used as a function argument

In Haskell 2010, certain kinds of expressions can be used without parentheses as an argument to an operator,
but not as an argument to a function. They include do, lambda, if, case, and let expressions. (also \case mdo)

> when (x > 0) do
>   print x
>   exitFailure

instead of

> when (x > 0) (do
>   print x
>   exitFailure)

> main :: IO ()
> main = do
>   print "Hello"
