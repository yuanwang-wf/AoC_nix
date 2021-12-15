{-# LANGUAGE TupleSections #-}

module Y2021.Day8 (day8PartI, entry) where

import Control.Arrow ((&&&))
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

type Signal = String

type Entry = ([Signal], [Signal])

readEntry :: String -> Entry
readEntry = ((_f . head) &&& (_f . (!! 1))) . splitOn "|"
  where
    _f = splitOn " "

day8Input :: IO [Entry]
day8Input = fmap readEntry . lines <$> readFile "data/2021/day8.txt"

solveD8P1 :: [Entry] -> Int
solveD8P1 = length . concatMap (filter (\x -> length x `elem` [2, 3, 4, 7]) . snd)

day8PartI :: IO Int
day8PartI = solveD8P1 <$> day8Input

day8PartII :: IO Int
day8PartII = sum . map solveD8P2 <$> day8Input

solveD8P2 :: Entry -> Int
solveD8P2 = undefined

isSolved :: M.Map (S.Set Char) (S.Set Char) -> Entry -> Bool
isSolved m = all (flip M.member m . S.fromList) . snd

digitMap :: M.Map (S.Set Char) Int
digitMap =
  M.fromList
    [ (S.fromList "abcefg", 0),
      (S.fromList "cf", 1),
      (S.fromList "acdeg", 2),
      (S.fromList "acdfg", 3),
      (S.fromList "bcdf", 4),
      (S.fromList "abdfg", 5),
      (S.fromList "abdefg", 6),
      (S.fromList "acf", 7),
      (S.fromList "abcdefg", 8),
      (S.fromList "abcdfg", 9)
    ]

uniqueMap :: M.Map Int (S.Set Char)
uniqueMap =
  M.fromList
    [ (2, S.fromList "cf"),
      (3, S.fromList "acf"),
      (4, S.fromList "bcdf")
    ]

entry :: Entry
entry =
  ( [ "acedgfb",
      "cdfbe",
      "gcdfa",
      "fbcad",
      "dab",
      "cefabd",
      "cdfgeb",
      "eafb",
      "cagedb",
      "ab"
    ],
    ["cdfeb", "fcadb", "cdfeb", "cdbaf"]
  )

initMap :: Entry -> M.Map (S.Set Char) (S.Set Char)
initMap = M.fromList . mapMaybe (\x -> (\y -> (S.fromList x, y)) <$> M.lookup (length x) uniqueMap) . fst
