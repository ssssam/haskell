import Data.List
import System.Environment (getArgs)

-- Ch. 04 Exercise 3

getFirstWordsStep :: [String] -> [String]
getFirstWordsStep [] = []
getFirstWordsStep (l:ls) =
    let ws = words l in
    case ws of
        [] -> getFirstWordsStep ls
        ws -> head (ws) : getFirstWordsStep ls

getFirstWords :: String -> String
getFirstWords input =
    unlines (getFirstWordsStep lines)
    where lines = splitLines input

-- Ch. 04 Exercise 4

transposeText :: String -> String
transposeText text = unlines $ transpose $ lines text

--

splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in pre : case suf of
                  ('\r':'\n':rest) -> splitLines rest
                  ('\r':rest)      -> splitLines rest
                  ('\n':rest)      -> splitLines rest
                  _                -> []

isLineTerminator c = c == '\r' || c =='\n'


interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
                [input,output] -> interactWith function input output
                _ -> putStrLn "Error: exactly two arguments needed"

          myFunction = transposeText

