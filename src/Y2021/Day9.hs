module Y2021.Day9 (day9PartI, day9PartII) where

import Control.Monad ((<=<))
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Ord (Down(Down))
import qualified Data.Set as S

type HeightMap = [[Int]]

type Point = (Int, Int)

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

lowPoints :: HeightMap -> [Point]
lowPoints m =
  [ (x, y) | x <- [0 .. max_raw], y <- [0 .. max_col], all (> m !! x !! y) (adjPoint x y m)
  ]
  where
    max_col = (length . head) m - 1
    max_raw = length m - 1

solveDay9PartI :: HeightMap -> Int
solveDay9PartI m = (sum . fmap (\(x, y) -> m !! x !! y + 1)) (lowPoints m)

neighbors :: Point -> HeightMap -> [Point]
neighbors (x, y) m = catMaybes [up, left, down, right]
  where
    up :: Maybe Point
    up = if x == 0 then Nothing else Just (x -1, y)

    down :: Maybe Point
    down = if x == max_raw then Nothing else Just (x + 1, y)

    left :: Maybe Point
    left = if y == 0 then Nothing else Just (x, y -1)

    right :: Maybe Point
    right = if y == max_col then Nothing else Just (x, y + 1)

    max_col = (length . head) m - 1
    max_raw = length m - 1

getValue :: Point -> HeightMap -> Int
getValue (x, y) m = m !! x !! y

-- cannot get list version to work
basin :: HeightMap -> Point -> S.Set Point
basin m p = helper m (S.singleton p) [p' | p' <- neighbors p m, p' /= p, getValue p' m /= 9]
  where
    helper :: HeightMap -> S.Set Point -> [Point] -> S.Set Point
    helper m territory [] = territory
    helper m territory frontiers =
      let   t' = S.union territory (S.fromList frontiers)
            nF = [ n |p' <- frontiers,  n <- neighbors p' m, S.notMember n  t', getValue n m /= 9]
       in helper m t' nF

solveDay9PartII :: HeightMap -> Int
solveDay9PartII m  = product . take 3 .sortOn Down $ [ length (basin m p)   |   p <- lowPoints m]

-- data Max3 a = Max3 {getLargest :: Maybe a, getSecLargest :: Maybe a, get3rd :: Maybe a} deriving (Eq, Ord, Read, Show, Bounded)

-- instance (Num a, Ord a, Bounded a) => Monoid (Max3 a) where
--   mempty = Max3 minBound minBound minBound
--   (Max3 a1 a2 a3) `mappend`  (Max3 _ _ _) = _

-- instance Foldable Max3 where
--   foldMap f (Max3 a1 a2 a3) = f a1 <> f a2 <> f a3

day9PartI :: IO Int
day9PartI = solveDay9PartI <$> example

day9PartII :: IO Int
day9PartII = solveDay9PartII <$> getMap "day9.txt"
