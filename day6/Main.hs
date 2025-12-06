import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

parseNumbers :: [String] -> [Int]
parseNumbers = map (\x -> read x :: Int)

calculateExpression :: (String, [Int]) -> Int
calculateExpression ("+", xs) = sum xs
calculateExpression ("*", xs) = product xs

parseTransposed :: Int -> [String] -> [String]
parseTransposed index lines
  | index == -1 = []
  | otherwise = currentNumber : parseTransposed (index - 1) lines
  where
    currentNumber = filter isDigit (map (!! index) lines)

solve :: [String] -> String -> Int
solve operandLines operationsLine =
  let numbers = map (parseNumbers . words) operandLines
      expressions = zip (words operationsLine) (transpose numbers)
   in sum $ map calculateExpression expressions

solve2 :: [String] -> String -> Int
solve2 operandLines operationsLine =
  let numbers = map parseNumbers $ splitWhen (== "") $ parseTransposed (length operationsLine - 1) operandLines
      expressions = zip (reverse $ words operationsLine) numbers
   in sum $ map calculateExpression expressions

main = do
  input <- readFile "input.txt"
  let (strNumbers, operations) = fromMaybe (error "Invalid input") $ unsnoc $ lines input
  print $ solve strNumbers operations
  print $ solve2 strNumbers operations
