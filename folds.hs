import Data.Char

-- Chapter 4 Exercise 2.1

-- why would foldl be the correct thing to use here ????

asInt_fold :: String -> Int
asInt_fold cs =
    fst (foldr convert (0, 1) cs)
    where convert c (result, base) = (result + digitToInt c * base, base * 10)

asInt_fold2 :: String -> Int
asInt_fold2 cs =
    case cs of
        '-' : cs -> - asInt cs
        cs -> asInt cs
    where
        asInt = fst . foldr convert (0, 1)
        convert c (result, base) = (result + digitToInt c * base, base * 10)

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
 
