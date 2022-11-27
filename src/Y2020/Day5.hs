module Y2020.Day5 (partI, partII) where

import Control.Applicative (liftA2)
import Data.Char (digitToInt)
import Data.List ((\\))
import Data.Maybe (catMaybes, listToMaybe)
import Numeric (readInt)

type SeatId = Int

--https://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
-- https://en.wikipedia.org/wiki/Nota_bene
-- NB: readInt is the "dual" of showIntAtBase, and readDec is the "dual" of showInt. The inconsistent naming is a historical accident.
readBin :: String -> Maybe Int
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

--FBFBBFF
decodeRow :: String -> String
decodeRow = map (\c -> if c == 'F' then '0' else '1')

decodeCol :: String -> String
decodeCol = map (\c -> if c == 'L' then '0' else '1')

seatId :: String -> Maybe SeatId
seatId content = liftA2 (+) (fmap (* 8) . readBin . decodeRow $ row) (readBin . decodeCol $ column)
  where
    (row, column) = splitAt 7 content

maxSeat :: [String] -> SeatId
maxSeat = maximum . catMaybes . fmap seatId

findSeat :: [String] -> SeatId
findSeat input = head d
  where
    seats = catMaybes . fmap seatId $ input
    l = minimum seats
    h = maximum seats
    d = [l .. h] \\ seats

partI :: IO SeatId
partI = do
    content <- readFile "/data/2020/day5.txt"
    return . maxSeat . lines $ content

partII :: IO SeatId
partII = do
    content <- readFile "data/2020/day5.txt"
    return . findSeat . lines $ content
