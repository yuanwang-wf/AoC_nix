module Y2021.Day1 where

readInput :: FilePath -> IO [Int]
readInput path = map readNum . lines <$> readFile path
  where
    readNum :: String -> Int
    readNum = read

getNums :: IO [Int]
getNums = readInput "data/day1.txt"

partI :: IO ()
partI = do
  nums <- getNums
  let m = fst $ foldl (\(counter, prev) num -> (counter + if maybe False ( < num) prev  then 1 else 0 , Just num)) (0, Nothing) nums
  print m

partII :: IO ()
partII = do
  nums <- getNums
  print (myFold nums)

myFold :: [Int] -> Int
myFold nums = length  [i | i <- [0.. length nums - 4 ], nums !! i < nums !! (i+3)]
