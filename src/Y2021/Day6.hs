module Y2021.Day6 (day6PartI, day6PartII) where

import Control.Monad.ST (runST)
import Data.List.Split (splitOn)
import Data.STRef (newSTRef, readSTRef, writeSTRef)

data FS = FS {d0 :: Integer, d1 :: Integer, d2 :: Integer, d3 :: Integer, d4 :: Integer, d5 :: Integer, d6 :: Integer, d7 :: Integer, d8 :: Integer} deriving (Show)

empty :: FS
empty = FS 0 0 0 0 0 0 0 0 0

value :: FS -> Integer
value fs = d0 fs + d1 fs + d2 fs + d3 fs + d4 fs + d5 fs + d6 fs + d7 fs + d8 fs

accumFS :: Int -> FS -> FS
accumFS 0 fs = fs{d0 = d0 fs + 1}
accumFS 1 fs = fs{d1 = d1 fs + 1}
accumFS 2 fs = fs{d2 = d2 fs + 1}
accumFS 3 fs = fs{d3 = d3 fs + 1}
accumFS 4 fs = fs{d4 = d4 fs + 1}
accumFS 5 fs = fs{d5 = d5 fs + 1}
accumFS 6 fs = fs{d6 = d6 fs + 1}
accumFS 7 fs = fs{d7 = d7 fs + 1}
accumFS _ fs = fs

startState :: FS
startState = FS 0 1 1 2 1 0 0 0 0 --[3,4,3,1,2]

shift :: FS -> FS
shift fs = fs{d0 = d1 fs, d1 = d2 fs, d2 = d3 fs, d3 = d4 fs, d4 = d5 fs, d5 = d6 fs, d6 = d7 fs + d0 fs, d7 = d8 fs, d8 = d0 fs}

day6Input :: IO FS
day6Input = foldr (accumFS . (read :: String -> Int)) empty . splitOn "," <$> readFile "data/2021/day6.txt"

fishST :: Integer -> FS -> FS
fishST day state = runST $ do
    x <- newSTRef state
    fishST' day x
  where
    fishST' 0 x = readSTRef x
    fishST' n x = do
        x' <- readSTRef x
        writeSTRef x $! shift x'
        fishST' (n - 1) x

day6PartI :: IO Integer
day6PartI = value . fishST 80 <$> day6Input

day6PartII :: IO Integer
day6PartII = value . fishST 256 <$> day6Input
