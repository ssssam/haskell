-- Ch. 4 exercise 1

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just (tail xs)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

-- Ch. 4 exercise 2

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith predicate [] = []
splitWith predicate xs = 
    case break (not . predicate) xs of 
      ([], rest) -> (splitWith predicate (tail (rest)))
      (first, []) -> [first]
      (first, rest) -> first : (splitWith predicate (tail (rest)))

