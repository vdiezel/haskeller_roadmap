Functional zippers

Data structure to navigate and modify complex, nested data structures
like trees.

They provide two feature: a focused element and maintaining context
so that we "know" where we currently are in the within the data structure

They remind me a little bit of pointers to an element.
This example stems from ChatGPT and I'm not sure if this is correct
because I had to manually fix a couple things here which where obviously wrong

> data Tree a = Empty | Node a (Tree a) (Tree a)
> data TreeZipper a = TreeZipper (Tree a) [Tree a]

> goLeft :: TreeZipper a -> Maybe (TreeZipper a)
> goLeft (TreeZipper (Node x l r) breadcrumbs) = Just (TreeZipper l (Node x l r: breadcrumbs))
> goLeft _ = Nothing
>
> goRight :: TreeZipper a -> Maybe (TreeZipper a)
> goRight (TreeZipper (Node x l r) breadcrumbs) = Just (TreeZipper r (Node x l r: breadcrumbs))
> goRight _ = Nothing
>
> goUp :: TreeZipper a -> Maybe (TreeZipper a)
> goUp (TreeZipper focus []) = Nothing
> goUp (TreeZipper focus (parent : breadcrumbs)) = Just (TreeZipper parent breadcrumbs)

> main :: IO ()
> main = print "Hello Word"
