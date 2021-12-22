-- |

module Y2021.Day10 where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
lookupM :: Map.Map Char Char
lookupM = Map.fromList [('(',')'),('[',']'),('{','}'),('<','>')]

illegalCharacter :: String -> Maybe Char
illegalCharacter = helper []
  where helper :: String -> String -> Maybe Char
        helper _ [] = Nothing
        helper leftC (x:xs)
          | Map.member x lookupM = helper (x:leftC) xs
          | null leftC || Map.lookup (head leftC) lookupM /= Just x = Just x
          | otherwise = helper (tail leftC) xs

pointMap :: Map.Map Char Int
pointMap = Map.fromList [(')', 3),(']', 57),('}', 1197),('>', 25137)]

solveDay10PartI :: String -> Int
solveDay10PartI  = foldr agg 0 .   fmap illegalCharacter . lines
  where agg :: Maybe Char -> Int -> Int
        agg x y = fromMaybe 0 (x >>= flip Map.lookup pointMap) + y


day10PartI :: IO Int
day10PartI = solveDay10PartI <$> readFile "data/2021/day10.txt"
