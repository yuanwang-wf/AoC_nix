
module Y2020.Day1 where

import Data.Maybe (fromJust)
import qualified Data.Set as Set

-- https://adventofcode.com/2020/day/1
-- Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.
-- For example, suppose your expense report contained the following:

-- we can implement this using list comp
-- TODO fix the bug, use index
twoSum :: [Int] -> Int
twoSum nums =
  head
    [ i * j | i <- nums, j <- nums, i + j == 2020
    ]

twoSum' :: [Int] -> Maybe Int
twoSum' nums = snd $ foldr f (Set.empty, Nothing :: Maybe Int) nums
  where
    f :: Int -> (Set.Set Int, Maybe Int) -> (Set.Set Int, Maybe Int)
    f num (s, acum) = case acum of
      Just _ -> (s, acum)
      Nothing -> if Set.member (2020 - num) s then (s, Just (num * (2020 - num))) else (Set.insert num s, Nothing)

getInput :: IO [Int]
getInput = do
  content <- readFile "data/2020/day1.txt"
  return (map read $ lines content)

solution :: IO ()
solution = do
  input <- getInput
  print . fromJust . twoSum' $ input

solution' :: IO ()
solution' = do
  input <- getInput
  print . twoSum $ input

threeSum :: [Int] -> Int
threeSum nums =
  head [i * j * k | i <- nums, j <- nums, k <- nums, i + j + k == 2020]
