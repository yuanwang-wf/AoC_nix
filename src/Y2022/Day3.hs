module Y2022.Day3 (testInput, solution1, partI) where

import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

priorityMap :: Map.Map Char Int
priorityMap = Map.fromList (zip ['a' .. 'z'] [1 .. 26] ++ zip ['A' .. 'Z'] [27 .. 52])

testInput :: String
testInput =
    "\
    \vJrwpWtwJgWrhcsFMMfFFhFp\n\
    \jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
    \PmmdzqPrVvPwwTWBwg\n\
    \wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
    \ttgJtRGJQctTZtZT\n\
    \CrZsJsPPZsGzwwsLwLmpwMDw\n"

type Rucksack = String

split :: String -> (Rucksack, Rucksack)
split input = splitAt a input
  where
    a = length input `div` 2

shareItems :: (Rucksack, Rucksack) -> Rucksack
-- shareItems (first, sec) = [ i | i <- first,  i `elem` sec ]
shareItems (f, s) = foldr (\x y -> if (x `elem` s && not (x `elem` y)) then (x : y) else y) "" f
solution1 :: String -> Int
solution1 input = foldr (\x y -> (fromMaybe 0 (Map.lookup x priorityMap)) + y) 0 . concat . map (shareItems . split) . lines $ input

partI :: IO Int
partI = solution1 <$> readFile "data/2022/day3.txt"
