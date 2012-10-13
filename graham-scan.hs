import Data.List

data Direction = LeftTurn | RightTurn | StraightLine
                 deriving (Show, Eq)

data Point = Point {
                 x :: Double,
                 y :: Double
             }
             deriving (Show, Eq)

-- Exercise 10

turnDirection :: Point -> Point -> Point -> Direction

turnDirection a b c =
        let direction = (x b - x a) * (y c - y a) -
                        (y b - y a) * (x c - x a)
        in if direction < 0 then LeftTurn
           else if direction > 0 then RightTurn
           else StraightLine


-- Exercise 11

turnDirectionList :: [Point] -> [Direction]

turnDirectionList (a:b:c:rest) =
        ((turnDirection a b c) : (turnDirectionList (b : c : rest)))
turnDirectionList _ = error "Insufficient points" 

-- Exercise 12

lowerPoint :: Point -> Point -> Point
lowestPoint :: [Point] -> Point

lowerPoint a b =
        if y a < y b then a
        else if y a > y b then b
        else if x a < x b then a
        else b

lowestPoint (a:b:rest) | rest /= [] =
        lowerPoint (lowerPoint a b) (lowestPoint (b : rest))
lowestPoint (a:b:[]) =
        lowerPoint a b
lowestPoint (a:[]) = a


angleAgainstX :: Point -> Point -> Double

angleAgainstX a b = atan opposite / adjacent
        where opposite = (x b - x a)
              adjacent = (y b - y a)

compareByAngleFromPoint :: Point -> Point -> Point -> Ordering

compareByAngleFromPoint lowest a b =
        if angleAgainstX lowest a < angleAgainstX lowest b then LT
        else if angleAgainstX lowest a > angleAgainstX lowest b then GT
        else EQ


-- Basically, drop any right turns ....

grahamScan :: [Point] -> [Point]

grahamScan (a:b:c:rest) | rest /= [] = 
        if direction == RightTurn then
                (grahamScan (b : c : rest))
        else b : (grahamScan (b : c : rest))
        where direction = turnDirection a b c 
grahamScan (a:b:c:[]) =
        if direction == RightTurn then [c]
        else (b : [c])
        where direction = turnDirection a b c 

convexHull :: [Point] -> [Point]

convexHull points =
        p : grahamScan (p : sortBy (compareByAngleFromPoint p) (delete p points))
        where p = (lowestPoint points)



-- These two functions are actually of no use.

-- Calculate direction of line formed by a-b-c

distance :: Point -> Point -> Double

distance a b = (sqrt (((x b) - (x a)) ** 2 + ((y b) - (y a)) ** 2))

-- Calculate angle between lines (ab) and (bc), using the cosine law

turn :: Point -> Point -> Point -> Double

turn pointA pointB pointC
    | pointA == pointB || pointB == pointC =
        error "Points overlap"
turn pointA pointB pointC =
        acos (((lengthAB ** 2) + (lengthBC ** 2) - (lengthAC **2)) /
              (2 * lengthAB * lengthBC))
        where lengthAB = distance pointA pointB
              lengthBC = distance pointB pointC
              lengthAC = distance pointA pointC

