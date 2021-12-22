-- |
module Y2021.Day10 where

import Data.Either (rights)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust)

lookupM :: Map.Map Char Char
lookupM = Map.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

illegalCharacter :: String -> Maybe Char
illegalCharacter = helper []
  where
    helper :: String -> String -> Maybe Char
    helper _ [] = Nothing
    helper leftC (x : xs)
      | Map.member x lookupM = helper (x : leftC) xs
      | null leftC || Map.lookup (head leftC) lookupM /= Just x = Just x
      | otherwise = helper (tail leftC) xs

pointMap :: Map.Map Char Int
pointMap = Map.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

inCompletePointMap :: Map.Map Char Int
inCompletePointMap = Map.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]

solveDay10PartI :: String -> Int
solveDay10PartI = foldr agg 0 . fmap illegalCharacter . lines
  where
    agg :: Maybe Char -> Int -> Int
    agg x y = fromMaybe 0 (x >>= flip Map.lookup pointMap) + y

day10PartI :: IO Int
day10PartI = solveDay10PartI <$> readFile "data/2021/day10.txt"

illegalCharacter' :: String -> Either Char String
illegalCharacter' = helper []
  where
    helper :: String -> String -> Either Char String
    helper leftC [] = Right leftC
    helper leftC (x : xs)
      | Map.member x lookupM = helper (x : leftC) xs
      | null leftC || Map.lookup (head leftC) lookupM /= Just x = Left x
      | otherwise = helper (tail leftC) xs

solveDay10PartII :: String -> Int
solveDay10PartII = median . fmap (agg . autocomplete) . rights . fmap illegalCharacter' . lines
  where
    autocomplete :: String -> String
    autocomplete = foldl (flip (:)) "" . fmap fromJust . filter isJust . fmap (`Map.lookup` lookupM)

    agg :: String -> Int
    agg = foldr (\x y -> fromMaybe 0 (Map.lookup x inCompletePointMap) + y * 5) 0

    -- There will always be an odd number of scores to consider.
    -- quick selection
    median :: (Ord a) => [a] -> a
    median x = sort x !! (n `div` 2)
      where
        n = length x

day10PartII :: IO Int
day10PartII = solveDay10PartII <$> readFile "data/2021/day10.txt"
