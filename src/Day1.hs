module Day1 where

readInput :: FilePath -> IO [Int]
readInput path = (map readNum . lines) <$> readFile path
  where
    readNum :: String -> Int
    readNum = read

partI :: IO ()
partI = do
  nums <- readInput "data/input1.txt"
  print nums
