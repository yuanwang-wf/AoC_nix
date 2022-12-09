{-# LANGUAGE OverloadedStrings #-}
module Y2022.Day7 where

import Data.Attoparsec.Text hiding (take)
import Data.Text qualified as T
import Control.Applicative (many, (<|>))
data FileSystem = Directory {   _dName :: String, _content :: [FileSystem], _parent :: Maybe FileSystem } | File { _fName :: String , _size :: Int }

getSize :: FileSystem -> Int
getSize (File _ s ) = s
getSize (Directory _ [] _ ) = 0
getSize (Directory n (x:xs) p ) = getSize x + getSize (Directory n xs p )

data Output = CD String | LS | Dir String | FL Int String deriving Show

parserFile :: Parser Output
parserFile = do
  size <- many1 digit
  space
  name <- many1 (letter <|> char '.' <|> digit)
  return $ FL (read size)  name

outputParser :: Parser Output
-- outputParser = (string "$ ls" >> return LS) <|> (string "$ cd " >>  CD <$> manyTill anyChar (string "\n") )  <|> (string "dir " >>  Dir <$> manyTill anyChar (string "\n") ) <|> parserFile

outputParser = (string "$ ls" >> return LS) <|> (string "$ cd " >>  CD  <$> many (letter <|> char '/' <|> char '.' ))  <|> (string "dir " >>  Dir  <$> many (letter <|> digit)) <|> parserFile

outputsParser :: Parser [Output]
outputsParser = many (outputParser <* endOfLine   )

test :: IO ()
test = do

    input <- readFile "data/2022/day7.txt"
    print   ( length .lines $ input)
    -- print $ map (parseOnly outputParser . T.pack) $ lines input
    print $ fmap length . parseOnly outputsParser $ T.pack input

    -- print $ parseOnly outputParser "$ cd /"
    -- print $ parseOnly outputParser "$ ls"
    -- print $ parseOnly outputParser "14779 cmss"
    -- print $ parseOnly outputParser "dir ctctt"
    -- print $ parseOnly outputParser "101350 gpbswq.njr"
    -- print $ parseOnly outputParser "270744 mglrchsr"

buildTree :: [Output] -> FileSystem -> FileSystem
buildTree = undefined
