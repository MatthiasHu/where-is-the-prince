module Main where

import Control.Monad
import Data.List (nub)

type Room = Int
data Dir  = L | R deriving (Show,Eq)

allRooms :: Int -> [Room]
allRooms n = [0..n-1]

allPrincessStrategies :: Int -> Int -> [[Room]]
allPrincessStrategies n k = replicateM k (allRooms n)

isWinningStrategy :: Int -> [Room] -> Bool
isWinningStrategy n xs = null $
  foldr (\x -> nub . concatMap (evade x)) (allRooms n) xs
  where
    evade :: Room -> Room -> [Room]
    evade princess prince =
      filter (/= princess) . filter (\x -> 0<=x && x<n) $ [prince-1, prince+1]

shortestWinningStrategies :: Int -> [[Room]]
shortestWinningStrategies n = go 1
    where
    go k = if null ss then go (k+1) else ss
        where ss = filter (isWinningStrategy n) (allPrincessStrategies n k)

bestWinningStrategy :: Int -> [Room]
bestWinningStrategy = head . shortestWinningStrategies

main :: IO ()
main = forM_ [1..] $ print . shortestWinningStrategies
