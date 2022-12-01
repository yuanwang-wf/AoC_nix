module Y2022.Day1 (what) where

import Data.List.Split

what :: FilePath -> IO [[String]]
what path =  map  lines  . splitOn "\n\n" <$> readFile path
