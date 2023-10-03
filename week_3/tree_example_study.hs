-- trying to grasp the Functor, Applicative, Foldable, Traversable business

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Empty = Empty
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

myTree :: Tree Int
myTree = Node 5 (Node 6 Empty Empty) (Node 1 Empty Empty)

increaseByOne :: Tree Int -> Tree Int
increaseByOne = fmap (+1)

-- so far, so easy

-- A tree is not really great for applicative context
-- as it is rather a container than a computation or effect
-- that can be applied to values, but just for the sake of
-- it I'm implementing it. It is not required for Foldable
-- or Traversable

instance Applicative Tree where
  pure :: a -> Tree a
  pure val = Node val Empty Empty
  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  Empty <*> _ = Empty
  _ <*> Empty = Empty
  (Node f fl fr) <*> (Node x xl xr) = Node (f x) (fl <*> xl) (fr <*> xr)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ acc Empty = acc
  foldr f acc (Node x l r) = foldr f (f x (foldr f acc l)) r

-- I love this.

getSum :: Tree Int -> Int
getSum = sum

toList :: Tree Int -> [Int]
-- same as foldr (\x, acc -> x : cc) [] tree
-- toList tree = foldr (\x acc -> x : acc) [] tree
toList = foldr (\x acc -> x : acc) []

-- because this requires Applicative, I suspect
-- this is again not really useful for a tree?

-- instance Traversable Tree where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

main :: IO ()
main = do
  print (show (increaseByOne myTree))
  print (show (sum myTree))
  print (show (toList myTree))
