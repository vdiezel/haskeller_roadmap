Data.Int and Data.Word

Data.Int

It provide various x-bit integers to work with, Int8, Int16, Int16 and Int32

> import Data.Int
> import Data.Word

> intValue :: Int32
> intValue = 42

Data.Word

Provides unsigned integers of different bit sizes, Word8, Word16, Word32, Word64

> wordValue :: Word16
> wordValue = 65535

These are useful when interacting with extenal systems that specify the size and
signedness of integer data.

For arbitrarly big numbers, use the "Integer" type.

> main :: IO ()
> main = print "Hello Word"
