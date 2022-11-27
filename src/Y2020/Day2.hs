{-# LANGUAGE OverloadedStrings #-}

module Y2020.Day2 where

-- import Control.Applicative (Alternative (many))
import Data.Attoparsec.Text
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

-- We need a parser to parse password policy
-- 1-3 a: abcde
-- num dash num: and a string
-- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec

data PWRecord = PWRecord {c :: Char, minL :: Int, maxL :: Int, pw :: T.Text}

validPassword :: PWRecord -> Bool
validPassword r = cnt >= minL r && cnt <= maxL r
  where
    cnt = T.foldr (\c' s -> if c' == c r then s + 1 else s) 0 (pw r)

validPassword' :: PWRecord -> Bool
validPassword' (PWRecord c' l r w) = f w l c' /= f w r c'
  where
    f :: T.Text -> Int -> Char -> Bool
    f t i c'' = (i <= T.length t) && (T.index t (i - 1) == c'')

parseRecord :: Parser PWRecord
parseRecord =
    do
        minLength <- decimal
        char '-'
        maxLength <- decimal
        char ' '
        c' <- anyChar
        string ": "
        PWRecord c' minLength maxLength <$> takeText

-- TODO implement this correctly
--recordParser :: Parser [PWRecord]
--recordParser = many $ parseRecord <* endOfInput

--test :: IO ()
--test = print . (fmap validPassword) $ parseOnly parseRecord "15-19 k: kkkkkkkkkkkkzkkkkkkk"

solution :: IO ()
solution = do
    content <- TIO.readFile "data/2020/day2.txt"
    print $ foldr (f . fmap validPassword . parseOnly parseRecord) (0 :: Int) (T.lines content)
  where
    f (Right True) s = s + 1
    f _ s = s

solution' :: IO ()
solution' = do
    content <- TIO.readFile "data/day2.txt"
    print $ foldr (f . fmap validPassword' . parseOnly parseRecord) (0 :: Int) (T.lines content)
  where
    f (Right True) s = s + 1
    f _ s = s
