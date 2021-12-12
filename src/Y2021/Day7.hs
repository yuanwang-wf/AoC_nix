module Y2021.Day7 (day7PartII, day7PartI) where


import Data.List (sort)

day7Input :: IO [Int]
day7Input = read . (\x -> "[" ++ x ++ "]") <$> readFile "data/2021/day7.txt"

-- quick selection
median :: (Ord a, Fractional a) => [a] -> a
median x =
   if odd n
     then sort x !! (n `div` 2)
     else ((sort x !! (n `div` 2 - 1)) + (sort x !! (n `div` 2))) / 2
    where n = length x


findCost :: Int -> [Int] -> Int
findCost num  = sum . fmap ( abs . (-) num)

findCost' :: Int -> [Int] -> Int
findCost' num  = sum . fmap ((\x -> (x*(x+1)) `div` 2) . ( abs . (-) num))

findAllCost :: [Int] -> [Int]
findAllCost nums = fmap (`findCost` nums) [0 .. maximum nums]

findAllCost' :: [Int] -> [Int]
findAllCost' nums = fmap (`findCost'` nums) [0 .. maximum nums]

day7PartII :: IO Int
day7PartII = minimum . findAllCost' <$> day7Input

day7PartI :: IO Double
day7PartI = do
  nums <- map fromIntegral <$>  day7Input
  let m = median  nums
      cost = sum . fmap (\x -> abs (x - m)) $ nums
  pure cost

-- day7PartII :: IO Double
-- day7PartII = do
--   nums <- map fromIntegral <$>  day7Input
--   let m = median nums
--       cost = sum . fmap ((\x -> (x * (x+1)) / 2) . (\x -> abs (x - m))) $ nums
--   pure cost
