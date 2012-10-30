import Data.Char
import Data.List

-- Chapter 4 Exercise 2.1

-- why would foldl be the correct thing to use here ????

asInt_fold :: String -> Int
asInt_fold cs =
    fst (foldr convert (0, 1) cs)
    where convert c (result, base) = (result + digitToInt c * base, base * 10)

-- Exercise 2.2

asInt_fold2 :: String -> Int
asInt_fold2 cs =
    case cs of
        '-' : cs -> - asInt cs
        cs -> asInt cs
    where
        asInt = fst . foldr convert (0, 1)
        convert c (result, base) = (result + digitToInt c * base, base * 10)

-- Exercise 2.3

-- Would be nice to use pattern matching instead, but how do you call a
-- nested function before the where/let clause?
asInt_fold3 :: String -> Int
asInt_fold3 cs =
    case cs of
        "" -> error "No number"
        "-" -> error "No number"
        '-' : cs -> - asInt cs
        cs -> asInt cs
    where
        asInt = fst . foldr convert (0, 1)
        convert c (i, base)
            | isDigit c = let i' = i + digitToInt c * base
                          in if i' >= i then (i', base * 10) else error "Overflow"
            | otherwise = error ("Not a digit: " ++ [c])

-- Exercise 2.4

-- (foldl is the correct thing to remove to get rid of the 'base' parameter :)

type ErrorMessage = String

asInt_convert4 :: (Either ErrorMessage Int) -> Char -> (Either ErrorMessage Int)
asInt_convert4 (Left err) _ = Left err
asInt_convert4 (Right i) char
    | isDigit char =
        let i' = i * 10 + digitToInt char
        in if i' >= i then Right i' else Left "Overflow"
    | otherwise = Left ("Not a digit: " ++ [char])

asInt_fold4 :: String -> (Either ErrorMessage Int)
asInt_fold4 "" = Left "No number"
asInt_fold4 "-" = Left "No number"
asInt_fold4 cs =
    case cs of 
        '-' : cs -> 
            let result = foldl asInt_convert4 (Right 0) cs
            in case result of
                Left a -> Left a
                Right a -> Right (- a)
        cs ->
            let result = foldl asInt_convert4 (Right 0) cs
            in case result of
                Left a -> Left a
                Right a -> Right a
-- Exercise 2.5

-- Seriously? This isn't an exercise.

-- Exercise 2.6

myconcat :: [[a]] -> [a]
myconcat lists = foldr concat_step [] lists
                 where concat_step a rest = a ++ rest

asInt_tests_failure = ["", "-", "-x", "foo", "1.3x", "2147483648"]
asInt_tests_success = ["0", "-20", "2147483647"]

-- Exercise 2.7

takeWhile_recursive :: (a -> Bool) -> [a] -> [a]
takeWhile_recursive _ [] = []
takeWhile_recursive f (x:xs) = x : if f x then takeWhile f xs else []

takeWhile_foldr :: (a -> Bool) -> [a] -> [a]
takeWhile_foldr f xs =
    foldr (step f) [] xs
    where step f x rest = if f x then x:rest else [] 

-- Exercise 2.8 + 2.9

-- Doesn't behave the same as groupBy when used with eg. (<) as
-- the comparator

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy compare xs =
    foldr (step compare) [] xs
    where
        step _ x [] = [[x]]
        step compare x rest =
            if compare x (head (head rest)) then
                (x : (head rest)) : tail rest
            else
                [x] : rest


-- Exercise 2.10

-- any: True if predicate is True for any of list
-- foldl is more appropriate to avoid seeking to end of list, but still,
-- fold is the wrong thing here because we don't need to look through the
-- whole list, we just need to find the first value where predicate is True
myAny :: (a -> Bool) -> [a] -> Bool
myAny predicate xs = foldl' (step predicate) False xs
    where step predicate first x = if predicate x then True else False

-- cycle: Infinite repeat list
-- can't be done; foldl only iterates the original list.
myCycle :: [a] -> [a]
myCycle xs = foldl' step xs []
    where step first xs = xs ++ xs

-- words: Split string into words
-- using foldl is much harder because list construction occurs at the
-- end of the list rather than the start
-- still, prepending space breaks function
myWords :: String -> [String] 
myWords cs = foldr step [] cs 
    where
        step c []
            | isSpace c = []
            | otherwise = [[c]]
        step c words
            | isSpace c = [] : words
            | otherwise = (c : (head words)) : tail words 

-- unlines: Join together with '\n'
myUnlines :: [String] -> String
myUnlines lines = foldr step "" lines
    where
        step line "" = line
        step line string = line ++ "\n" ++ string
