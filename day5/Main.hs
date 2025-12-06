import Data.List (sort)

data Range = Range Int Int

data RangePoint = Opened Int | Closed Int deriving (Eq, Show)

instance Ord RangePoint where
    compare x y = compare (cmp x) (cmp y)
      where
        cmp (Opened a) = (a, 1)
        cmp (Closed a) = (a, -1)

(->.) (Range from to) = [Opened from, Closed (to + 1)]

(<.>) x y = abs $ toInt y - toInt x
  where
    toInt (Opened a) = a
    toInt (Closed a) = a

solve :: [Int] -> [Range] -> Int
solve ids freshIds = length $ filter (\x -> any (inRange x) freshIds) ids

solve2 :: [Range] -> Int
solve2 ranges =
    let rangePoints = sort $ concatMap (->.) ranges
     in countUnion Nothing 0 rangePoints

inRange :: Int -> Range -> Bool
inRange x (Range from to) = x >= from && x <= to

parseIdRange :: String -> Range
parseIdRange s = Range (read from :: Int) (read (drop 1 to) :: Int)
  where
    (from, to) = span (/= '-') s

countUnion :: Maybe RangePoint -> Int -> [RangePoint] -> Int
countUnion _ _ [] = 0
countUnion Nothing openedRanges ((Closed _) : xs) = error "Invalid input"
countUnion Nothing openedRanges (x@(Opened _) : xs) = countUnion (Just x) 1 xs
countUnion (Just previousPosition) openedRanges (x : xs) = currentLength + countUnion (Just x) newOpenedRanges xs
  where
    currentLength = if openedRanges > 0 then x <.> previousPosition else 0
    newOpenedRanges =
        openedRanges + case x of
            Opened a -> 1
            Closed a -> -1

main :: IO ()
main = do
    input <- readFile "input.txt"
    let inputLines = lines input
    let freshIds = map parseIdRange $ takeWhile (/= "") inputLines
    let availableIds = map (\x -> read x :: Int) $ drop 1 $ dropWhile (/= "") inputLines

    print $ solve availableIds freshIds
    print $ solve2 freshIds
