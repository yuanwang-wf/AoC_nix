module Y2022.Day1 (partI, TopThree, foldThree, partII) where

import Data.List.Split ( splitOn )
import Data.Semigroup (Max(Max))
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.List (sortOn)
import Data.Ord (Down(Down))

what :: FilePath -> IO [[Int]]
what path = map (map readNum .  lines)  . splitOn "\n\n" <$> readFile path
  where
    readNum :: String -> Int
    readNum = read


partI :: IO ()
partI = do
  a <- what "data/2022/day1.txt"
  print (foldMap (Max .sum ) a)

partII :: IO ()
partII = do
  a <- what "data/2022/day1.txt"
  print $ (sumTopThree . foldThree . map  ( Just . sum)) a


data TopThree n = TopThree ( Maybe n)  ( Maybe n)   ( Maybe n)


appendTopThree :: (Ord n, Num n) => TopThree n -> TopThree n -> TopThree n
appendTopThree (TopThree x y z ) (TopThree x' y' z') = case nums of
  [] -> TopThree Nothing Nothing Nothing
  [a] -> TopThree (Just a) Nothing Nothing
  [a, b] -> TopThree (Just a) (Just b) Nothing
  -- [a, b, c] -> TopThree (Just a) (Just b) (Just c)
  (a:  b: c: _) -> TopThree (Just a) (Just b) (Just c)

  where
    nums =  (sortOn Down . map fromJust . filter isJust)  [x, y, z, x', y', z']


instance ( Ord n, Num n) => Semigroup (TopThree n) where
   (<> ) = appendTopThree

instance ( Ord n, Num n) => Monoid (TopThree n) where
  mempty = TopThree Nothing Nothing Nothing

foldThree :: (Ord n, Num n, Foldable  t) => t (Maybe n)-> TopThree n
foldThree a = (foldMap  TopThree ) a Nothing Nothing

sumTopThree :: TopThree Int -> Int
sumTopThree (TopThree x y z) = fromMaybe 0 x + fromMaybe 0 y + fromMaybe 0 z
