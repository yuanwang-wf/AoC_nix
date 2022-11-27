module Y2021.Day3 where

import Data.Char (digitToInt)
import Data.Maybe (listToMaybe)
import Numeric (readInt)

getDay3Input :: IO [String]
getDay3Input = lines <$> readFile "data/2021/day3.txt"

testInput :: [String]
testInput =
    [ "00100"
    , "11110"
    , "10110"
    , "10111"
    , "10101"
    , "01111"
    , "00111"
    , "11100"
    , "10000"
    , "11001"
    , "00010"
    , "01010"
    ]

findGamma :: [String] -> String
findGamma xs = [if sum i > 0 then '1' else '0' | i <- [0 .. (length . head $ xs) - 1]]
  where
    sum :: Int -> Int
    sum i = foldr (\c a -> if c == '0' then a - 1 else a + 1) 0 (map (\x -> x !! i) xs)

findEpsilon :: [String] -> String
findEpsilon = map (\c -> if c == '1' then '0' else '1') . findGamma

readBin :: String -> Int
readBin = fst . head . readInt 2 (`elem` "01") digitToInt

day3PartIPure :: [String] -> Int
day3PartIPure xs = gamma * epsilon
  where
    gamma = readBin . findGamma $ xs
    epsilon = readBin . findEpsilon $ xs

day3PartI :: IO Int
day3PartI = day3PartIPure <$> getDay3Input

day3PartII :: IO Int
day3PartII = do
    codes <- getDay3Input
    let oRating = readBin $ splitBy 0 codes (\x y -> length x >= length y)
        coRating = readBin $ splitBy 0 codes (\x y -> length x < length y) --why not <=
    pure (oRating * coRating)

splitBy :: Int -> [String] -> ([String] -> [String] -> Bool) -> String
splitBy i [x] fn = x
splitBy i xs fn =
    if fn ones zeros
        then splitBy (i + 1) ones fn
        else splitBy (i + 1) zeros fn
  where
    (ones, zeros) = _splitBy i xs

_splitBy :: Int -> [String] -> ([String], [String])
_splitBy i [] = ([], [])
_splitBy i (x : xs) = if x !! i == '1' then ([x] ++ (fst (_splitBy i xs)), snd (_splitBy i xs)) else (fst (_splitBy i xs), [x] ++ snd (_splitBy i xs))
