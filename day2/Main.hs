import Data.List
import Data.List.Split

parseIds :: [String] -> [Int]
parseIds (x:xs) = [lower..upper]
  where lower = read x :: Int
        upper = read (head xs) :: Int

isInvalid :: Int -> Bool
isInvalid n = 
  let nStr = show n
      halfLength = (length nStr) `div` 2
      left = take halfLength nStr
      right = drop halfLength nStr
  in right == left

isInvalid2 id = any allEqual (getAllSplits id)

getAllSplits n = 
  let nStr = show n
      lengths = [1..((length nStr) `div` 2)]
  in map (\x -> chunksOf x nStr) lengths

allEqual [] = False
allEqual (x:xs) = all (== x) xs


main :: IO ()
main = do
  input <- readFile "input.txt"
  let ids = concat $ map (parseIds .  splitOn "-") (splitOn "," input) 
 
  let result = filter isInvalid ids
  print (foldr (+) 0 result)
  --
  let result2 = filter isInvalid2 ids
  print (foldr (+) 0 result2)
