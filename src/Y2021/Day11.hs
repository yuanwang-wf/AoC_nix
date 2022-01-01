-- |
module Y2021.Day11 where

import Control.Monad (liftM, (<=<))
import Data.Char (digitToInt)
import qualified Data.Map as Map
import Data.Maybe (maybe)
import qualified Data.Set as Set
import Control.Monad.Fix (fix)

type Position = (Int, Int)

type Grid = Map.Map Position Int

readGrid :: String -> Grid
readGrid = Map.fromList . concatMap (\(x, y) -> [((x, i), digitToInt (y !! i)) | i <- [0 .. length y - 1]]) . zip [0 ..] . lines

example :: IO Grid
example = readGrid <$> readFile "data/2021/day11-test.txt"

realGrid :: IO Grid
realGrid = readGrid <$> readFile "data/2021/day11.txt"


-- narrow down
incr' :: Grid -> Grid
incr' = fmap (+1)

removeFlash :: Grid -> Maybe Grid
removeFlash g = if Map.null flashed then Nothing else Just (Map.difference (Map.unionWith (+) increments g ) flashed)
  where flashed = Map.filter (> 9) g
        entries = Map.fromListWith (+) [ (p', 1) | p <- Map.keys flashed, p' <- neighbors g p ]
        increments = Map.intersection entries g

-- why fix point won't work
removeFlashFix :: Grid -> Grid
removeFlashFix g = case removeFlash g of
  Nothing -> g
  Just map -> removeFlashFix map



incr :: Grid -> (Grid, Set.Set Position)
incr = (\(x, y) -> (Map.fromList x, y)) . foldr (\((x, y), v) (entries, s) -> if v == 9 then (((x, y), 0) : entries, (x, y) `Set.insert` s) else (((x, y), v + 1) : entries, s)) ([], Set.empty) . Map.assocs

flash :: Grid -> Position -> (Grid, Set.Set Position)
flash g p = foldr f (g, Set.empty) (neighbors g p)
  where
    updateGrid :: Grid -> Position -> (Grid, Maybe Position)
    updateGrid g p = (g', if g' Map.! p == 0 && g Map.! p /= 0 then Just p else Nothing)
      where
        g' = Map.adjust (\val -> if val == 9 || val == 0 then 0 else val + 1) p g

    f :: Position -> (Grid, Set.Set Position) -> (Grid, Set.Set Position)
    f p (g', points) = case updateGrid g' p of
      (g'', Nothing) -> (g'', points)
      (g'', Just p') -> let points' = p' `Set.insert` points in (\ (x,y) -> (x,  maybe points' (`Set.insert` points') y   )) (updateGrid g'' p')

step :: Grid -> (Grid, Int)
step g = (\(x, y) -> (x, Set.size y)) $ foldr f (g', points) points
  where
    f :: Position -> (Grid, Set.Set Position) -> (Grid, Set.Set Position)
    f p (g'', ps) = let (g''', ps') = flash g'' p in (g''', ps' `Set.union` ps)
    (g', points) = incr g

step' :: Grid -> (Grid, Int)
step' g = (Map.union g' (fmap (const 0) g), (Map.size g) - (Map.size g'))
  where g' = removeFlashFix (incr' g)

neighbors :: Grid -> Position -> [Position]
neighbors g (x, y) =
  filter
    (`Map.member` g)
    [ (x -1, y -1),
      (x -1, y),
      (x -1, y + 1),
      (x, y -1),
      (x, y + 1),
      (x + 1, y -1),
      (x + 1, y),
      (x + 1, y + 1)
    ]

-- could rewrite as unfold
solveDay11PartI :: Grid -> Int -> Int
solveDay11PartI g s = helper g s 0
  where
    helper :: Grid -> Int -> Int -> Int
    helper g 0 count = count
    helper g n count = let (g', c') = step' g in helper g' (n -1) (count + c')

day11PartI :: IO Int
day11PartI = (`solveDay11PartI` 100) <$> realGrid

solveDay11PartII :: Grid -> Int
solveDay11PartII g = helper g 0
  where
    helper :: Grid -> Int -> Int
    helper g n = let (g', c) = step' g in if c == Map.size g then (n+1) else helper g' (n+1)


day11PartII :: IO Int
day11PartII = solveDay11PartII  <$> realGrid
