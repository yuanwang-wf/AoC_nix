module Y2021.Day5 (day5PartII, day5PartI) where

import Control.Arrow ((&&&))
import Data.Array (Array, array, elems, (!), (//))
import Data.Bits (Bits (xor))
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as Map

type Point = (Int, Int)

type Vent = (Point, Point)

readVent :: String -> Vent
readVent = ((nums . (!! 0)) &&& (nums . (!! 1))) . splitOn "->"
  where
    nums :: String -> Point
    nums = ((readInt . (!! 0)) &&& (readInt . (!! 1))) . splitOn ","

    readInt :: String -> Int
    readInt = read

grid :: Array (Int, Int) Int
grid = array ((0, 0), (999, 999)) [((i, j), 0) | i <- [0 .. 999], j <- [0 .. 999]]

updateGrid :: Array (Int, Int) Int -> Point -> Point -> Array (Int, Int) Int
updateGrid g (a, b) (a', b') = g // [((i, j), 1 + (g ! (i, j))) | i <- [a .. a'], j <- [b .. b']]

partI :: [Vent] -> Int
partI = length . filter (>= 2) . elems . foldr (f . _sort) grid . _filter
  where
    _filter :: [Vent] -> [Vent]
    _filter = filter (\((a, b), (a', b')) -> (a == a') `xor` (b == b'))

    _sort :: Vent -> Vent
    _sort ((a, b), (a', b')) = ((min a a', min b b'), (max a a', max b b'))

    f :: Vent -> Array (Int, Int) Int -> Array (Int, Int) Int
    f v as = updateGrid as `uncurry` v

day5PartI :: IO Int
day5PartI = partI <$> content
  where
    content :: IO [Vent]
    content = map readVent . lines <$> readFile "data/2021/day5.txt"

range :: (Point, Point) -> [Point]
range x = if isNotDiagonal x then (straightRange . _sortS) x else (if isDiagonal x then (diagonalRange . _sortD) x else []) -- if possible to rewrite it with alernative
  where
    isNotDiagonal :: (Point, Point) -> Bool
    isNotDiagonal = \((a, b), (a', b')) -> (a == a') `xor` (b == b')

    isDiagonal :: (Point, Point) -> Bool
    isDiagonal = \((a, b), (a', b')) -> abs (a - a') == abs (b - b')

    _sortS :: Vent -> Vent
    _sortS ((a, b), (a', b')) = ((min a a', min b b'), (max a a', max b b'))

    _sortD :: Vent -> Vent
    _sortD ((a, b), (a', b')) = if a > a' then ((a', b'), (a, b)) else ((a, b), (a', b'))

    straightRange :: (Point, Point) -> [Point]
    straightRange ((a', b), (a'', b')) = [(i, j) | i <- [a' .. a''], j <- [b .. b']]

    diagonalRange :: (Point, Point) -> [Point]
    diagonalRange ((a', b), (a'', b')) = let o = if b > b' then (-) else (+) in [(a'' + i, b `o` i) | i <- [0 .. (a' - a'')]]

-- take me a while to relaized how bad is this method
-- we are updating grid too many times, and we do not need 1000*1000 grid to begin with
-- partII :: [Vent] -> Int
-- partII = length . filter (>= 2) .  elems . foldr f grid  . concatMap range
--   where
--         f :: Point -> Array (Int, Int) Int -> Array (Int, Int) Int
--         f (x,y) grid = grid // [((x,y), grid !(x,y) + 1  )]

partII :: [Vent] -> Int
partII = Map.foldr (\a b -> if a >= 2 then b + 1 else b) 0 . foldr f Map.empty . concatMap range
  where
    f :: Point -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
    f p m = Map.insertWith (+) p 1 m

day5PartII :: IO Int
day5PartII = partII <$> content
  where
    content :: IO [Vent]
    content = map readVent . lines <$> readFile "data/2021/day5.txt"
