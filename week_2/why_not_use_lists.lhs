Why not to use lists

Because they are inefficient for a lot of operations, as they are linked lists
so they often require O(n) time complexity.

Here are the complexities of the basic operations:

head: O(1) get first element
tail: O(1) get the tail
cons: O(1) append an element at the front
last: O(n) get the last element
init: O(n) get all elements except the last one
snoc: O(n) append an element to the end ("cons" spelled backwards)
(++): O(n) concat
(!!): O(n) access element by index

Vec(tor)

Not sure what "Vec" refers to here, as there is no built-in Vec type, only Vector.
Vector has the better docs, so I will talk about that.

https://hackage.haskell.org/package/vector-0.13.0.0/docs/Data-Vector.html

Comes in two flavours, mutatable and immutable.

(!): O(1) access element by index
head: O(1) get first element
slice: O(1) get the tail or init
last: O(1) get the last element
snoc: O(n) append an element to the end
cons: O(n) append an element at the front
(++): O(n) concat

Seq

https://hackage.haskell.org/package/containers-0.7/docs/Data-Sequence.html

Always finite, unlike lists.
Support more efficient operations, like constant time access for front and back,
and logarithmic concat, splitAt, drop and take, element access for insertion,
deleting and updating. I think they are based on finger trees or balanced binary trees.

They are typically slower than lists when using only operations for which they have the same
time complexity, like stacks.

Not as fast for fast indexing as vectors or arrays, but more optimized copying.

(lookup): O(log n) access element by index
take: O(log n) take n elements from the front (can be used for head)
drop: O(log n) drop n elements from the front (can be used for tail)
|>: O(1) append an element to the end
<|: O(1) append an element at the front
(><): O(log n) concat

Array

https://hackage.haskell.org/package/array-0.5.6.0/docs/Data-Array.html#v:array

Basic, non-strict, finite array. The docs do not give the time complexity of things,
but they give only a very small API. They provide constant time access to all elements.

Set

https://hackage.haskell.org/package/containers-0.7/docs/Data-Set.html

Sets are pretty self-explanatory. Efficient for insetion, deletion, union, intersection, difference
and membership tests. Usually O(log n) complexity. Elements are kept sorted, so the default
set is an ordered set, elements have to implement the Ord type class.

Use Red-Black Trees internally.

Map

https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html

Associative array/ dictionaries based on key-value pairs.
In Haskell, the key types seem to be unrestricted, expect that they have to implement "Ord".
based on sized-balanced binary trees. Most common operations, like lookup and insert/update/delete
are O(log n).

HashMap

https://hackage.haskell.org/package/hashmap-1.3.3/docs/Data-HashMap.html

A map that uses hash values for the keys.
According to the docs, time complexities are pretty much the same as map.

There is also Data.HashTable, with O(1) lookups, but it is a mutable data structure.
https://hackage.haskell.org/package/base-4.3.1.0/docs/Data-HashTable.html

> main :: IO ()
> main = print "Hello Word"
