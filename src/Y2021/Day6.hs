module Y2021.Day6 (day6PartI) where

import Control.Monad.State
    ( evalState, MonadState(put, get), State )
import Data.List.Split (splitOn)
-- Example use of State monad
-- Passes a string of dictionary {a,b,c}
-- Game is to produce a number from the string.
-- By default the game is off, a C toggles the
-- game on and off. A 'a' gives +1 and a b gives -1.
-- E.g
-- 'ab'    = 0
-- 'ca'    = 1
-- 'cabca' = 0
-- State = game is on or off & current score
--       = (Bool, Int)

-- type GameValue = Int
-- type GameState = (Bool, Int)

-- playGame :: String -> State GameState GameValue
-- playGame []     = do
--     (_, score) <- get
--     return score

-- playGame (x:xs) = do
--     (on, score) <- get
--     case x of
--          'a' | on -> put (on, score + 1)
--          'b' | on -> put (on, score - 1)
--          'c'      -> put (not on, score)
--          _        -> put (on, score)
--     playGame xs

-- startState :: (Bool, Int)
-- startState = (False, 0)
-- main = print $ evalState (playGame "abcaaacbbcabbab") startState


type FishState = [Int]
type FishValue = Int


playGame :: Int -> State FishState FishValue
playGame 0 = do
  fishs <- get
  pure . length $ fishs

playGame n = do
  s <- get
  put (day_ s)
  playGame (n -1)

  where
        day :: FishState -> FishState
        day  = sum_  . foldr (\n (fs, counter) -> if n == 0 then (fs ++ [6], counter +1) else (fs ++ [n-1], counter))   (([], 0) :: ([Int], Int))

        sum_ :: ([Int], Int) -> [Int]
        sum_ =  \(fs, counter) -> fs ++ (if counter == 0 then [] else [8| i <- [0.. ( counter -1)] ] :: [Int] )

        day_ :: FishState -> FishState
        day_ = foldr (\n fs -> if n == 0 then 6:8:fs else (n-1):fs )  []


startState :: FishState
startState = [3,4,3,1,2]

day6Input :: IO FishState
day6Input = map read .  splitOn "," <$> readFile "data/2021/day6.txt"

day6PartI :: IO ()
day6PartI = do
  state <- day6Input
  print $ evalState (playGame 256) startState
