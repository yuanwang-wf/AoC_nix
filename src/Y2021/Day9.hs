module Y2021.Day9 where

import Control.Monad ((<=<))
import Data.Maybe (catMaybes)
import Y2021.Day7 (day7PartII)

type HeightMap = [[Int]]

getMap :: FilePath -> IO HeightMap
getMap = (pure . (fmap . fmap) ((read :: String -> Int) . (: [])) . lines) <=< readFile . ("data/2021/" <>)

example :: IO HeightMap
example = getMap "day9-test.txt"

adjPoint :: Int -> Int -> HeightMap -> [Int]
adjPoint x y m = catMaybes [up, left, down, right]
  where
    up :: Maybe Int
    up = if x == 0 then Nothing else Just (m !! (x -1) !! y)

    down :: Maybe Int
    down = if x == max_raw then Nothing else Just (m !! (x + 1) !! y)

    left :: Maybe Int
    left = if y == 0 then Nothing else Just (m !! x !! (y -1))

    right :: Maybe Int
    right = if y == max_col then Nothing else Just (m !! x !! (y + 1))

    max_col = (length . head) m - 1
    max_raw = length m - 1

lowPoints :: HeightMap -> [Int]
lowPoints m =
  [ m !! x !! y | x <- [0 .. max_raw], y <- [0 .. max_col], all (> m !! x !! y) (adjPoint x y m)
  ]
  where
    max_col = (length . head) m - 1
    max_raw = length m - 1

solveDay9 :: HeightMap -> Int
solveDay9 = sum . fmap (+ 1) . lowPoints

day9PartI :: IO Int
day9PartI = solveDay9 <$> getMap "day9.txt"
