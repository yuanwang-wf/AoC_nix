module Y2022.Day7 where

import Data.Attoparsec.Text hiding (take)

data FileSystem = Directory {   _dName :: String, _content :: [File] } | File { _fName :: String , _size :: Int }

getSize :: FileSystem -> Int
getSize (File _ s ) = s
getSize (Directory _ [] ) = 0
getSize (Directory n (x:xs) ) = getSize x + getSize (Directory n xs)

data Command = CD | LS
