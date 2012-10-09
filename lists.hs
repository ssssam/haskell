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

-- Exercise 1

myLength :: [a] -> Int

myLength (x:xs) = (myLength xs) + 1
myLength [] = 0

-- Exercise 3

-- No polymorphic type to allow finding mean of list of Int?

mean :: [Double] -> Double
mean x = (sum x) / (fromIntegral (length x))

-- Exercise 4

palindrome :: [a] -> [a]
palindrome x = x ++ (reverse x)

-- Exercise 5


dropLast :: [a] -> [a]
dropLast x = (reverse (tail (reverse (x))))

-- isPalindrome :: [a] -> Bool
isPalindrome :: [Int] -> Bool
isPalindrome [] = True
isPalindrome x | length x == 1 = True
isPalindrome (x:xs) = if x /= (last xs)
                      then False
                      else (isPalindrome (dropLast xs))


