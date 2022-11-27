module Y2020.Day4 where

import Data.Char (isNumber)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Text.Read (readMaybe)

newtype Byr = Byr String deriving (Show)

newtype Iyr = Iyr String deriving (Show)

newtype Eyr = Eyr String deriving (Show)

newtype Hgt = Hgt String deriving (Show)

newtype Hcl = Hcl String deriving (Show)

newtype Ecl = Ecl String deriving (Show)

newtype Pid = Pid String deriving (Show)

newtype Cid = Cid String deriving (Show)

data Passport = Passport {byr :: Maybe Byr, iyr :: Maybe Iyr, eyr :: Maybe Eyr, hgt :: Maybe Hgt, hcl :: Maybe Hcl, ecl :: Maybe Ecl, pid :: Maybe Pid, cid :: Maybe Cid} deriving (Show)

parsePart :: String -> Passport -> Passport
parsePart ctn p = case splitOn ":" ctn of
    ["byr", x] -> p{byr = if validByr x then Just (Byr x) else Nothing}
    ["iyr", x] -> p{iyr = if validIyr x then Just (Iyr x) else Nothing}
    ["eyr", x] -> p{eyr = if validEyr x then Just (Eyr x) else Nothing}
    ["hgt", x] -> p{hgt = if validHgt x then Just (Hgt x) else Nothing}
    ["hcl", x] -> p{hcl = if validHcl x then Just (Hcl x) else Nothing}
    ["ecl", x] -> p{ecl = if validEcl x then Just (Ecl x) else Nothing}
    ["pid", x] -> p{pid = if validPid x then Just (Pid x) else Nothing}
    ["cid", x] -> p{cid = Just (Cid x)}
    _ -> p

validIntWithBound :: Int -> Int -> String -> Bool
validIntWithBound min max content = isJust ((readMaybe content :: Maybe Int) >>= (\v -> if v >= min && v <= max then Just v else Nothing))

validByr :: String -> Bool
validByr = validIntWithBound 1920 2002

validIyr :: String -> Bool
validIyr = validIntWithBound 2010 2020

validEyr :: String -> Bool
validEyr = validIntWithBound 2020 2030

validHgt :: String -> Bool
validHgt content = isSuffixOf "cm" content && validIntWithBound 150 193 (init (init content)) || (isSuffixOf "in" content && validIntWithBound 59 76 (init (init content)))

validHcl :: String -> Bool
validHcl content = length content == 7 && head content == '#' && foldr (\c b -> b && (c `elem` "#abcdef" || isNumber c)) True content

validEcl :: String -> Bool
validEcl "amb" = True
validEcl "blu" = True
validEcl "brn" = True
validEcl "gry" = True
validEcl "grn" = True
validEcl "hzl" = True
validEcl "oth" = True
validEcl _ = False

validPid :: String -> Bool
validPid content = length content == 9 && foldr (\c b -> b && isNumber c) True content

zero :: Passport
zero = Passport Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

parsePassport :: String -> Passport
parsePassport cnt = foldr parsePart zero $ splitOn "\n" cnt >>= splitOn " "

parsePassports :: String -> [Passport]
parsePassports = map parsePassport . splitOn "\n\n"

isValid :: Passport -> Bool
isValid p = foldr (\f v -> f p && v) True ([isJust . byr, isJust . iyr, isJust . eyr, isJust . hgt, isJust . hcl, isJust . ecl, isJust . pid] :: [Passport -> Bool])

getPassport :: IO [Passport]
getPassport = do
    content <- readFile "data/2020/day4.txt"
    return (parsePassports content)

test :: IO ()
test = do
    passports <- getPassport
    print $ foldr (\p sum -> if isValid p then sum + 1 else sum) (0 :: Int) passports
