module Y2022.Day5 where
import System.IO.Unsafe (unsafePerformIO)
import Data.List.Split (splitOn)
import Data.Char (isSpace)
type Crate = Char
type Stack = [Crate]
type Stacks = [Stack]
type Procedure = String

testInput :: String
testInput = unsafePerformIO ( readFile "data/2022/day5.txt")

splitStack :: String -> String
splitStack = head . splitOn "\n\n"

parseStack :: String -> Stacks
parseStack = snd . foldr (\ x y -> y) ([], []) . lines

parseHeader :: String -> [Int]
parseHeader input = foldr (\ (index, x) y -> if isSpace x then y else index: y ) [] (zip  [0..] input)
