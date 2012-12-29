-- Current functionality: run on your lastfm data dump and it
-- picks a random track for you! That's pretty fun!

import qualified Data.Map
import Random (randomRIO)
import System.Environment

parseArgs :: [String] -> String
parseArgs (filename:[]) = filename
parseArgs _ = error "Expected: path to dumpfile"

-- attempt #1 at writing split from scratch -- still a lot to learn!
split :: (a -> Bool) -> [a] -> [[a]]
split predicate (x:xs) =
        let rest = (split predicate xs)
        in if predicate x then
            ([] : rest)
        else
            ([x] ++ (head rest)) : (tail rest)
split predicate x = [x]

data Listen = Listen {
    timestamp :: Int,
    recordingName :: String,
    artistName :: String,
    albumName :: String,
    trackMBID :: String,
    artistMBID :: String,
    albumMBID :: String
} deriving (Show)

parseListen :: [String] -> Listen
parseListen (p1:p2:p3:p4:p5:p6:p7:[]) =
    Listen (read p1 :: Int) p2 p3 p4 p5 p6 p7
parseListen _ = error "Parse error"

parseDumpLine :: String -> Listen
parseDumpLine text = (parseListen (split ((==) '\t') text))

mapIdToArtist :: [Listen] -> Data.Map.Map String String
mapIdToArtist listens = foldr step (Data.Map.empty) listens
    where
        step listen map
            | (artistMBID listen) /= "" = 
                (Data.Map.insert (artistMBID listen) (artistName listen) map)
            | otherwise = map

main :: IO ()
main = do
    args <- getArgs
    let dumpfilename = parseArgs args
    contents <- readFile dumpfilename
    let listens = map parseDumpLine (lines contents)
    --let idToArtist = (mapIdToArtist listens)
    --print (Data.Map.lookup "e09983cb-182d-4e45-b085-664800953cf1" idToArtist)
    --print (Data.Map.lookup "9975369b-6fc1-455e-91d4-d3c8b1772e57" idToArtist)
    -- readFile dump
    listenNum <- randomRIO (0, (length listens) - 1)
    print (listens !! listenNum)

-- Things to actually do with this code
-- Index by artist
