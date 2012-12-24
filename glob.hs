-- Chapter 8

import Control.Exception.Base
import Control.Monad

type Candidate = (String, String)

-- I think shell globs match the literal string if they encounter an
-- unterminated character class. They certainly don't raise an exception.
parseSet :: String -> ([Char], String)
parseSet ('[':']':rest) = ([], rest)
parseSet ('[':x:'-':y:rest) = parseSet (x:'-':y:rest)
parseSet ('[':c:rest) = (c : (fst $ parseSet rest), (snd $ parseSet rest))
parseSet (']':rest) = ([], rest)
parseSet (x:'-':y:rest) = ([x..y] ++ (fst $ parseSet rest), (snd $ parseSet rest))
parseSet (c:"") = error "Unterminated character class"
parseSet (c:rest) = (c : (fst $ parseSet rest), (snd $ parseSet rest))

getFullyMatched :: [Candidate] -> [Candidate]
getFullyMatched = filter ((== "") . snd)

matchChars :: [Char] -> Candidate -> [Candidate] -> [Candidate]
matchChars  _ (string, "") matches = getFullyMatched matches
matchChars cs (string, unmatched) matches
    | (any ((==) (head unmatched)) cs) = (string, tail unmatched) : matches
    | otherwise = matches

globStep :: String -> [Candidate] -> [Candidate]

globStep pattern@('[':rest) xs =
    let (charset, rest) = parseSet pattern in
    globStep rest (foldr (matchChars charset) [] xs)

globStep ('*':goal:rest) xs =
    globStep rest $ map dropGoal $ dropUnmatched $ map matchUntil xs
    where
        matchUntil (string, unmatched) = (string, dropWhile (/=goal) unmatched)
        dropUnmatched = filter ((/= "") . snd)
        dropGoal x = (fst x, dropWhile (==goal) (snd x))

globStep ('*':[]) xs = xs

globStep ('?':rest) xs =
    globStep rest (foldr matchOne [] xs)
    where
        matchOne :: Candidate -> [Candidate] -> [Candidate]
        matchOne (string, "") matches = getFullyMatched matches
        matchOne (string, unmatched) matches = (string, tail unmatched) : matches

globStep (c:rest) xs =
    globStep rest (foldr (matchChars [c]) [] xs)

globStep "" xs = getFullyMatched xs

globFilter :: String -> [String] -> [String]
globFilter "" _ = []
globFilter pattern candidates =
    (map fst (globStep pattern (zip candidates candidates)))

assertMatch :: [String] -> String -> [String] -> IO ()
assertMatch corpus pattern expected =
    when (result /= expected)
         (throwIO $ AssertionFailed (
            "Assertion failed: '" ++ pattern ++ "', results: " ++ (show result)))
    where result = (globFilter pattern corpus)

main = do
    let corpus = ["af", "faf", "faff", "fail"]
    assertMatch corpus "faf" ["faf"]
    assertMatch corpus "fa[a-z][a-z]" ["faff", "fail"]
    assertMatch corpus "f*ail" ["fail"]
    assertMatch corpus "f*f" ["faf", "faff"]
    assertMatch corpus "?*f" ["af", "faf", "faff"]
