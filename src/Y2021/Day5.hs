module Y2021.Day5 where

import Control.Arrow ((&&&))
import Data.Array ( array, Array, (//), (!), elems )
import Data.List.Split (splitOn)
import Data.Data (ConstrRep(IntConstr))
import Data.Bits (Bits(xor))

type Point = (Int, Int)

type Vent = (Point, Point)

readVent :: [Char] -> Vent
readVent =  ((nums . (!! 0))   &&& (nums . (!! 1)))    . splitOn "->"
  where
        nums :: String -> Point
        nums =  ((readInt . (!! 0) ) &&& ( readInt  . (!! 1 )))   . splitOn ","

        readInt :: String -> Int
        readInt = read

grid :: Array (Int, Int) Int
grid = array ((0,0), (9, 9)) [((i,j), 0) | i <- [0..9], j <- [0..9]]

updateGrid :: Array (Int, Int) Int  -> Point -> Point -> Array (Int, Int) Int
updateGrid grid (a,b) (a', b') = grid // [ ((i,j),  1 + (grid ! (i,j)) )   | i <-  [a .. a'], j <- [b .. b'] ]

testVent :: [Vent]
testVent = [((0,9), (5,9)),
            ((8,0), (0,8)),
            ((9,4), (3,4)),
            ((2,2), (2,1)),
            ((7,0), (7,4)),
            ((6,4), (2,0)),
            ((0,9), (2,9)),
            ((3,4), (1,4)),
            ((0,0), (8,8)),
            ((5,5), (8,2))]

partI :: [Vent] -> Int
partI  =  length  . filter (>= 2)   .  elems . foldr f grid  . map _sort  . _filter
  where _filter :: [Vent] -> [Vent]
        _filter = filter (\((a,b), (a', b')) -> (a == a') `xor` (b == b'))

        _sort :: Vent -> Vent
        _sort ((a,b), (a',b')) = ((min a a', min b b'), (max a a', max b b'))

        f :: Vent -> Array (Int, Int) Int -> Array (Int, Int) Int
        f v as = updateGrid as `uncurry` v
