The difference between ByteString, String and Text.

Strings

Strings are a list of char, meaning a linked list.
Uses Unicode character encoding in utf-16 or utf-32.
A linked list is often not very convinient, e.g. appending and accessing elements
is not very performant. However when working with small texts and/or compatibility
with standard Haskell libraries is important.

Text (released 30.01.2010)

This is where Text comes in. It uses utf-16 encoding (making it more terse), and more efficient
implementation and great for performance sensitive applications. It is the default
choice for text processing in Haskell.
Text is strict, so the whole text is evaluated on construction which leads to a more
predictable memory usage. Full Unicode support, while String relies on Chars.
Text is using a "finger tree" as the internal data structure, making copying cheap.

What is a finger tree?
- From 1977 by Ralf Hinze and Ross Paterson
- basic operations in constant time or logarithmic time (amortized constand most of the time)
- random access
- splitting
- concatenation

Great video on them in Haskell
https://www.youtube.com/watch?v=ip92VMpf_-A

Also used for the Data.Sequence data type in Haskell.
They are called "finger" because they expose certain elements
for random access.

ByteString (19.04.2006, big improvement in 2009, which the default now)

Represents strings as bytes, which is very efficient, but requires knowledge
of the encoding to make sense out of them. Great for network communication and file I/O

> main :: IO ()
> main = print "Hello Word"

