module Y2022.Day1 (partI, TopThree, partII) where

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
  print $ (sumTopThree . foldThree . map sum ) a


newtype TopThree n = TopThree {getTopThere :: (Maybe n, Maybe n, Maybe n)}

toList :: TopThree n -> [n]
toList t = ( map fromJust . filter isJust ) [x, y, z]
  where
  (x, y , z) = getTopThere t

appendTopThree :: (Ord n, Num n) => TopThree n -> TopThree n -> TopThree n
appendTopThree l r = case nums of
  [] -> TopThree (Nothing, Nothing, Nothing)
  [a] -> TopThree (Just a, Nothing, Nothing)
  [a, b] -> TopThree (Just a, Just b, Nothing)
  -- [a, b, c] -> TopThree (Just a) (Just b) (Just c)
  (a:  b: c: _) -> TopThree (Just a , Just b, Just c)

  where
    nums =  sortOn Down  (toList l ++ toList r )


instance ( Ord n, Num n) => Semigroup (TopThree n) where
   (<> ) = appendTopThree

instance ( Ord n, Num n) => Monoid (TopThree n) where
  mempty = TopThree (Nothing ,Nothing, Nothing)

wrap :: a -> (Maybe a, Maybe a, Maybe a)
wrap x = (Just x, Nothing, Nothing )

foldThree :: (Ord n, Num n) => [n] -> TopThree n
foldThree  = foldMap  TopThree . map wrap

sumTopThree :: TopThree Int -> Int
sumTopThree = sum . toList
