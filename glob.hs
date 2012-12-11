-- Chapter 8

-- Naive glob implementation; receives a list of candidates and
-- recursively processes the whole list at once, pairing each
-- candidate with its remaining unmatched portion in a tuple.

-- Does not yet support [..] or **

type Candidate = (String, String)

-- *
matchUntil :: Char -> Candidate -> Candidate
matchUntil goal (string, unmatched) =
    (string, dropWhile (/=goal) unmatched)

-- . (for foldr)

matchOne (string, "") matches = matches
matchOne (string, unmatched) matches = (string, tail unmatched) : matches

-- char (for foldr)
matchChar c (string, "") matches = matches
matchChar c (string, unmatched) matches
    | (head unmatched) == c = (string, tail unmatched) : matches
    | otherwise = matches

globStep :: String -> [Candidate] -> [Candidate]
globStep ('*':q:rest) xs =
    globStep (q:rest) (map (matchUntil q) xs)
globStep ('*':[]) xs = xs
globStep ('.':rest) xs =
    globStep rest (foldr matchOne [] xs)
globStep (c:rest) xs =
    globStep rest (foldr (matchChar c) [] xs)
globStep "" xs = xs

globFilter :: String -> [String] -> [String]
globFilter "" _ = []
globFilter pattern candidates =
    (map fst (globStep pattern (zip candidates candidates)))

main = do
    print $ globFilter ".*f" ["af", "faf", "faff", "fail"]

