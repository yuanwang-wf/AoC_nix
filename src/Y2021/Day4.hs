module Y2021.Day4 where

import Control.Arrow ((&&&))
import Data.List.Split (splitOn)

type Pos = (Int, Bool)

type Grid = [[Pos]]

getDay4Input :: IO ([Int], [Grid])
getDay4Input = (readMove &&& readBoards) <$> part
  where
    part :: IO [[String]]
    part = splitOn [""] . lines <$> readFile "data/2021/day4-test.txt"

    readBoard :: [String] -> [[Pos]]
    readBoard = fmap (fmap ((\n -> (n, False) :: Pos) . read) . filter (/= "") . splitOn " ") -- numbers are pad on left
    readBoards :: [[String]] -> [Grid]
    readBoards = fmap readBoard . tail

    readMove :: [[String]] -> [Int]
    readMove = fmap read . splitOn "," . head . head
