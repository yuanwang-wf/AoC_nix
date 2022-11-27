module Y2020.Day6 (
    partI,
    partII,
) where

import Data.List.Split (splitOn)
import Data.Set qualified as Set
import GHC.Base (join)

getInput :: IO [String]
getInput = do
    fmap (splitOn "\n\n") $ readFile "data/2020/day6.txt"

getGroupAnswer :: String -> Set.Set Char
getGroupAnswer = foldr Set.insert Set.empty . join . lines

getGroupAnswer' :: String -> Set.Set Char
getGroupAnswer' = foldl1 Set.intersection . map (foldr Set.insert Set.empty) . lines

-- there is foldr1 foldl1
--intersections :: Ord a => [Set.Set a] -> Set.Set a
--intersections [] = Set.empty
--intersections [x] = x
--intersections (x : y : xs) = Set.intersection x (intersections (y : xs))

partI :: IO Int
partI = fmap (sum . (map (Set.size . getGroupAnswer))) getInput

partII :: IO Int
partII = fmap (sum . (map (Set.size . getGroupAnswer'))) getInput
