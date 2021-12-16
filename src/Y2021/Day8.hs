{-# LANGUAGE TupleSections #-}

module Y2021.Day8 (day8PartI, entry, initMap, extendMap, canBeZero, canBeSix, canBeNine, debug) where

import Control.Arrow ((&&&))
import Data.Bifunctor (bimap)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

type Signal = String

type Entry = ([Signal], [Signal])

type Mapping = M.Map (S.Set Char) (S.Set Char)

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
initMap = M.fromList . mapMaybe (\x ->  (S.fromList x, ) <$> M.lookup (length x) uniqueMap) . fst

canBe :: Int -> Char -> M.Map (S.Set Char) (S.Set Char) -> String -> Bool
canBe l c m str = length str == l &&  all (\(_, y) -> c `S.notMember` y)  (filter (\(x,_) -> (s `S.intersection` x) == x )  es)
  where es = M.assocs m
        s = S.fromList str

canBeZero :: M.Map (S.Set Char) (S.Set Char) -> String -> Bool
canBeZero = canBe 6 'd'

canBeSix :: M.Map (S.Set Char) (S.Set Char) -> String -> Bool
canBeSix = canBe 6 'c'

canBeNine :: M.Map (S.Set Char) (S.Set Char) -> String -> Bool
canBeNine = canBe 6 'e'

canBeTwo :: Mapping -> String -> Bool
canBeTwo s m = canBe 5 'b' s m && canBe 5 'f' s m

canBeThree :: Mapping -> String -> Bool
canBeThree s m = canBe 5 'b' s m && canBe 5 'e' s m

canBeFive :: Mapping -> String -> Bool
canBeFive s m = canBe 5 'c' s m && canBe 5 'e' s m

-- maybe we need to do fix point
extendMap :: M.Map (S.Set Char) (S.Set Char) -> M.Map (S.Set Char) (S.Set Char)
extendMap m = m `M.union` (M.fromList . filter (not . S.null . fst )  ) newEntires
  where
    es = M.assocs m
    newEntires =
      [ ( fst (es !! i) `S.difference` fst (es !! j) , snd (es !! i) `S.difference` snd (es !! j) )
        | i <- [0 .. length es - 1],
          j <- [0 .. length es -1]
      ]

iterateMap :: Mapping -> String -> Mapping
iterateMap m s = case length s of
  6 -> iterateMapOn6 m s
  5 -> iterateMapOn5 m s
  _ -> m


iterateMapOn6 :: Mapping -> String -> Mapping
iterateMapOn6 m s = case entryM m s of
  ((True, s'), (False, _), (False, _)) -> extendMap $ M.insert (S.fromList s) s' m
  ((False, _), (True, s'), (False, _)) -> extendMap $ M.insert (S.fromList s) s' m
  ((False, _), (False, _), (True, s')) -> extendMap $ M.insert (S.fromList s) s' m
  _ -> m
  where entryM :: Mapping -> String -> ((Bool, S.Set Char), (Bool, S.Set Char), (Bool, S.Set Char))
        entryM m s = ((canBeZero m s, S.fromList "abcefg"), (canBeSix m s, S.fromList "abdefg"), (canBeNine m s, S.fromList "abcdfg"))

iterateMapOn5 :: Mapping -> String -> Mapping
iterateMapOn5 m s = case entryM m s of
  ((True, s'), (False, _), (False, _)) -> M.insert (S.fromList s) s' m
  ((False, _), (True, s'), (False, _)) -> M.insert (S.fromList s) s' m
  ((False, _), (False, _), (True, s')) -> M.insert (S.fromList s) s' m
  _ -> m
  where entryM :: Mapping -> String -> ((Bool, S.Set Char), (Bool, S.Set Char), (Bool, S.Set Char))
        entryM m s = ((canBeTwo m s, S.fromList "acdeg"), (canBeThree m s, S.fromList "acdeg"), (canBeFive m s, S.fromList "abdfg"))

tryToSolve :: Entry -> Mapping
tryToSolve e =  foldr (flip iterateMap) m (fst e)
  where m = extendMap $ initMap e

isSolved :: Mapping -> String -> (Bool, Int)
isSolved m s = case length s of
  2 -> (True, 1)
  4 -> (True, 4)
  3 -> (True, 7)
  7 -> (True, 8)
  5 -> case solveOn5 s of
    ((True, v), (False, _), (False, _)) -> (True, v)
    ((False, _), (True, v), (False, _)) -> (True, v)
    ((False, _), (False, _), (True, v)) -> (True, v)
    _ -> (False, 0)

  6 -> case solveOn6 s of
    ((True, v), (False, _), (False, _)) -> (True, v)
    ((False, _), (True, v), (False, _)) -> (True, v)
    ((False, _), (False, _), (True, v)) -> (True, v)
    _ -> (False, 0)

  _ -> (False, 0)

  where solveOn5 :: String -> ((Bool, Int), (Bool, Int), (Bool, Int))
        solveOn5 x = ((canBeTwo m x, 2), (canBeThree m x, 3), (canBeFive m x, 5))

        solveOn6 :: String -> ((Bool, Int), (Bool, Int), (Bool, Int))
        solveOn6 x = ((canBeZero m x, 0), (canBeSix m x, 6), (canBeNine m x, 9))

debug :: IO ()
debug = do
  let m = tryToSolve entry
  print m
  print $ fmap (\x -> (x, length x, fst (isSolved m x), snd (isSolved m x))) (fst entry)
