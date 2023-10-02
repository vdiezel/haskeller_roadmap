OverloadedLists

Allows programmers to use the list notations to construct structures
like Map, IntMap, Vector, Text and Array.

> {-# LANGUAGE OverloadedLists #-}

> -- ['0'..'9']             :: Set Char
> -- [1..10]                :: Vector Int
> -- [("default",0), (k1,v1)] :: Map String Int
> -- ['a'..'z']             :: Text

This works using the isList class, which is exported from GHC.Exts.

> -- class IsList l where
> --   type Item l
>
> --   fromList :: [Item l] -> l
> --   toList   :: l -> [Item l]
>
> --   fromListN :: Int -> [Item l] -> l
> --   fromListN _ = fromList

> main :: IO ()
> main = print "Hello Word"
