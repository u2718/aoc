import Data.List
import Data.Maybe

countPapers :: Int -> Int -> [[Char]] -> Int
countPapers row column xs =
  let items =
        [ xs <!> (row, column - 1)
        , xs <!> (row - 1, column - 1)
        , xs <!> (row - 1, column)
        , xs <!> (row - 1, column + 1)
        , xs <!> (row, column + 1)
        , xs <!> (row + 1, column + 1)
        , xs <!> (row + 1, column)
        , xs <!> (row + 1, column - 1)
        ]
   in length $ filter (== '@') (map (fromMaybe '.') items)

(<!>) :: [[a]] -> (Int, Int) -> Maybe a
(<!>) xs (row, column) = fromMaybe [] (xs !? row) !? column

solve :: [[Char]] -> [[Char]]
solve xs =
  let (_, newXs) = removePapers (length xs - 1) (length (xs !! 0) - 1) xs
   in newXs

solve2 :: [[Char]] -> [[Char]]
solve2 xs =
  let (wasRemoved, newXs) = removePapers (length xs - 1) (length (xs !! 0) - 1) xs
   in if wasRemoved
        then solve2 newXs
        else xs

removePapers :: Int -> Int -> [[Char]] -> (Bool, [[Char]])
removePapers row column xs
  | row == 0 && column == 0 = (wasRemoved, [[result]])
  | column == 0 = do
      let (isNextRemoved, newXs) = removePapers (row - 1) (length (xs !! 0) - 1) xs
      (wasRemoved || isNextRemoved, [result] : newXs)
  | otherwise = do
      let (isNextRemoved, newXs) = removePapers row (column - 1) xs
      let (newHead : newTail) = newXs
      (wasRemoved || isNextRemoved, (result : newHead) : newTail)
 where
  currentItem = xs !! row !! column
  wasRemoved = currentItem == '@' && countPapers row column xs < 4
  result = if wasRemoved then 'x' else currentItem

main :: IO ()
main = do
  input <- readFile "input.txt"
  let grid = lines input

  let result = sum $ map (length . filter (== 'x')) (solve grid)
  print result

  let result2 = sum $ map (length . filter (== 'x')) (solve2 grid)
  print result2
