module Y2021.Day4 (day4PartI, day4PartII) where

import Control.Arrow ((&&&))
import Control.Monad (join)
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

type Pos = (Int, Bool)

type Grid = [[Pos]]

getDay4Input :: IO ([Int], [Grid])
getDay4Input = (readMove &&& readBoards) <$> part
  where
    part :: IO [[String]]
    part = splitOn [""] . lines <$> readFile "data/2021/day4.txt"

    readBoard :: [String] -> [[Pos]]
    readBoard = fmap (fmap ((\n -> (n, False) :: Pos) . read) . filter (/= "") . splitOn " ") -- numbers are pad on left
    readBoards :: [[String]] -> [Grid]
    readBoards = fmap readBoard . tail

    readMove :: [[String]] -> [Int]
    readMove = fmap read . splitOn "," . head . head

isWin :: Grid -> Bool
isWin = uncurry (||) . (rowCheck &&& columnCheck)
  where
    rowCheck :: [[Pos]] -> Bool
    rowCheck = any (all snd)

    columnCheck :: [[Pos]] -> Bool
    columnCheck = rowCheck . transpose

-- we do not need find corridation and update by index
move :: Int -> [Grid] -> [Grid]
move num = (fmap . fmap . fmap) (go num)
  where
    go :: Int -> Pos -> Pos
    go num (x, y) = if num == x then (x, True) else (x, y)

findScore :: [Int] -> [Grid] -> Maybe Int
findScore [] grids = Nothing
findScore (x : xs) grids = if null winGrids then findScore xs grids' else Just (x * sumOfUnMarked (head winGrids))
  where
    grids' = move x grids
    winGrids = filter isWin grids'

findLastScore :: [Int] -> [Grid] -> Maybe Int
findLastScore [] grids = Nothing
findLastScore (x : xs) [grid] = if (isWin . head) (move x [grid]) then (Just . (* x) . sumOfUnMarked . head) $ move x [grid] else findLastScore xs $ move x [grid]
findLastScore (x : xs) grids = if null winGrids then findLastScore xs grids' else findLastScore xs $ filter (not . isWin) grids'
  where
    grids' = move x grids
    winGrids = filter isWin grids'

sumOfUnMarked :: Grid -> Int
sumOfUnMarked = sum . fmap (\(x, y) -> if y then 0 else x) . join

day4PartI :: IO Int
day4PartI = fromJust . uncurry findScore <$> getDay4Input

day4PartII :: IO Int
day4PartII = fromJust . uncurry findLastScore <$> getDay4Input
