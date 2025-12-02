parseMove :: String -> (Char, Int)
parseMove (x:xs) = (x, read xs :: Int)

rotate :: Int -> (Char, Int) -> Int
rotate n ('L', distance) = (n - distance) `mod` 100
rotate n ('R', distance) = (n + distance) `mod` 100

rotate2 :: (Int, Int) -> (Char, Int) -> (Int, Int)
rotate2 (_, n) ('L', distance) = (abs (newValue `div` 100) - (if n == 0 then 1 else 0) + (if result == 0 then 1 else 0), result)
  where newValue = n - distance
        result = newValue `mod` 100
rotate2 (_, n) ('R', distance) = (newValue `div` 100, newValue `mod` 100)
  where newValue = n + distance

main :: IO ()
main = do
  input <- readFile "input.txt"
  let instructions = map parseMove . lines $ input
  
  let result = scanl rotate 50 instructions 
  let result2 = scanl rotate2 (0, 50) instructions 

  print . length . filter (==0) $ result 
  print . foldr (+) 0 $ map fst result2 
