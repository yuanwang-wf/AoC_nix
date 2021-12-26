-- |

module Y2021.Day11 where


import Control.Monad ((<=<), liftM)
import Data.Char (digitToInt)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (maybe)

type Position = (Int, Int)
type Grid = Map.Map Position Int

readGrid :: String  ->  Grid
readGrid  =  Map.fromList . concatMap (\(x, y) -> [((x,i), digitToInt  (y !! i) ) | i <- [0..length y - 1]]) . zip [0..] .  lines

example :: IO Grid
example = readGrid <$> readFile  "data/2021/day11-test.txt"


incr :: Grid -> (Grid, Set.Set Position)
incr  = (\(x,y) -> (Map.fromList x, y))  . foldr (\((x, y), v) (entries, s) -> if v == 9 then (((x,y), 0):entries, (x,y) `Set.insert` s )   else (((x,y), v+1):entries  ,s) ) ([], Set.empty )  . Map.assocs

flash :: Grid -> Position -> (Grid, Set.Set Position)
flash g p = foldr f (g, Set.empty ) (neighbors g p)
  where updateGrid :: Grid -> Position -> (Grid, Maybe Position)
        updateGrid g p = (g', if g' Map.!  p == 0 && g Map.! p /= 0 then Just p else Nothing )
          where g' = Map.adjust (\val -> if val == 9 || val == 0 then 0 else val + 1) p g

        f :: Position -> (Grid, Set.Set Position) -> (Grid, Set.Set Position)
        f p (g', points) = let (g'', mp) = updateGrid g' p in (g'', maybe points (`Set.insert` points) mp)

step :: Grid -> (Grid, Int)
step g = (\(x, y) -> (x, Set.size y))   $ foldr f (g', points ) points
  where

       f :: Position -> (Grid, Set.Set Position) -> (Grid, Set.Set Position)
       f p (g'', ps) = let (g''', ps') = flash g'' p in (g''', ps' `Set.union` ps)
       (g', points) = incr g

neighbors :: Grid -> Position -> [Position]
neighbors g (x,y) = filter (`Map.member` g)  [ (x-1, y-1), (x-1, y), (x-1, y+1),
                                               (x,y-1), (x, y+1),
                                               (x+1, y -1), (x+1, y) , (x+1, y+1)]

solveDay11PartI :: Grid -> Int -> Int
solveDay11PartI g s = helper g s 0
  where helper :: Grid -> Int -> Int -> Int
        helper g 0 count = count
        helper g n count = let (g', c') = step g in helper g' (n-1) (count + c')
