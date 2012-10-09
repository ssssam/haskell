data List a = Cons a (List a)| Nil
              deriving (Show)

fromList :: [a] -> List a

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

-- Exercise 1

toList :: List a -> [a]

toList (Cons x xs) = x:(toList xs)
toList Nil = []

-- Exercise 2

data Tree a = Tree (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)

