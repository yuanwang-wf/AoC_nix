module Y2022.Day5 where

import Data.Char (isSpace)
import Data.List.Split (splitOn)
import System.IO.Unsafe (unsafePerformIO)

type Crate = Char
type Stack = [Crate]
type Stacks = [Stack]
type Procedure = String

testInput :: String
testInput = unsafePerformIO (readFile "data/2022/day5.txt")

testStack :: Stacks
testStack = parseStack . splitStack $ testInput

testHeader :: ([Int], Stacks)
testHeader = parseHeader (last . lines . splitStack $ testInput)

splitStack :: String -> String
splitStack = head . splitOn "\n\n"

parseStack :: String -> Stacks
parseStack = snd . foldr parse' ([], []) . lines
  where
    parse' :: String -> ([Int], [Stack]) -> ([Int], [Stack])
    parse' input (indices, rows) = if null indices then parseHeader input else parseRow input (indices, rows)

    parseHeader :: String -> ([Int], Stacks)
    parseHeader input = (indices, stacks)
      where
        indices = foldr (\(index, x) y -> if isSpace x then y else index : y) [] (zip [0 ..] input)
        stacks = [[] | _ <- indices]
    parseRow :: String -> ([Int], [Stack]) -> ([Int], [Stack])
    parseRow input (indices, rows) = (indices, newRows)
      where
        row = [lookUp input index | index <- indices]

        lookUp :: String -> Int -> Maybe Crate
        lookUp x i = if (i < length x) then Just (x !! i) >>= (\a -> if isSpace a then Nothing else Just a) else Nothing

        newRows = map (\(x, y) -> maybe y (: y) x) (zip row rows)
