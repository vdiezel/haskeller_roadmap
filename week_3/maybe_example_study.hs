data MyMaybe a = MNothing | MJust a deriving (Show)

instance Functor MyMaybe where
  fmap f MNothing = MNothing
  fmap f (MJust v) = MJust (f v)

-- this is not useful yet; only together with Applicative this
-- should kick in!

instance Applicative MyMaybe where
  pure = MJust
  (MJust f) <*> (MJust y) = MJust (f y)
  _ <*> _ = MNothing

-- now we can chain two maybes

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

res1 :: MyMaybe Int
res1 = (+) <$> MJust 1 <*> MJust 2

-- It looks like I cannot chain an arbitrary amount of Applicatives
-- because the passed function has to have a fixed arity?

res2 :: MyMaybe Int
res2 = addThree <$> MJust 1 <*> MJust 2 <*> MJust 3

-- the problem: get the sum of a list of MMaybe
-- solution: use fold!

myList :: [MyMaybe Int]
myList = [MJust 1, MJust 2, MJust 3]

myList2 :: [MyMaybe Int]
myList2 = [MJust 1, MNothing, MJust 3]

res3 :: MyMaybe Int
res3 = foldr (\x acc -> (+) <$> x <*> acc) (MJust 0) myList

res4 :: MyMaybe Int
res4 = foldr (\x acc -> (+) <$> x <*> acc) (MJust 0) myList2

-- instance Foldable MyMaybe where
--   foldr :: (a -> b -> b) -> b -> MyMaybe a -> b
--   foldr _ acc MNothing = acc
--   foldr f acc (MJust val) = f val acc

main :: IO ()
main = do
  print (show res1)
  print (show res2)
  print (show res3)
  print (show res4)
