import Data.Char
import Data.List
import Data.Maybe

parseLine :: String -> [Int]
parseLine = map digitToInt

maxi :: [Int] -> (Int, Int)
maxi xs = (value, index)
 where
  value = maximum xs
  index = fromMaybe undefined $ elemIndex value xs

solve :: [Int] -> Int
solve xs = first * 10 + second
 where
  (first, firstIndex) = maxi $ init xs
  second = maximum (drop (firstIndex + 1) xs)

solve2 :: [Int] -> Int
solve2 xs = listToInt $ getMaxValues 12 xs

-- [1,2,3,4] -> 1234
listToInt :: [Int] -> Int
listToInt = foldl (\a b -> a * 10 + b) 0

getMaxValues :: Int -> [Int] -> [Int]
getMaxValues 0 xs = []
getMaxValues n xs = max : getMaxValues (n - 1) newXs
 where
  (max, index) = findNthMax n xs
  newXs = drop (index + 1) xs

findNthMax :: Int -> [Int] -> (Int, Int)
findNthMax n xs = (max, index)
 where
  len = length xs
  listPartToSearch = take (len - n + 1) xs
  (max, index) = maxi listPartToSearch

main :: IO ()
main = do
  input <- readFile "input.txt"
  let banks = map parseLine $ lines input

  let result = map solve banks
  print $ sum result

  let result2 = map solve2 banks
  print $ sum result2
