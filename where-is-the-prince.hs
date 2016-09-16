{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

type Room = Int
data Dir  = L | R deriving (Show,Eq)

-- Take the set of rooms where the prince could currently be
-- as a node of a graph.
type Node = Set.Set Room

allRooms :: Int -> [Room]
allRooms n = [0..n-1]

neighbours :: Int -> Node -> [(Room, Node)]
neighbours n node = do
  princess <- allRooms n
  let newNode = Set.fromList . concatMap (evade n princess) $ Set.toList node
  return (princess, newNode)

evade :: Int -> Room -> Room -> [Room]
evade n princess prince =
  filter (/= princess) . filter (\x -> 0<=x && x<n) $ [prince-1, prince+1]

-- modified Dijkstra algotizhm to list the traces (type [t])
-- of all shortest paths from a to b
dijkstra :: forall n t. Ord n => (n -> [(t, n)]) -> n -> n -> [[t]]
dijkstra neighbs a b = map reverse $ go Set.empty (Map.singleton a [[]])
  where
    go :: Set.Set n -> Map.Map n [[t]] -> [[t]]
    go out reached | b `Map.member` reached  = reached Map.! b
                   | otherwise  = go newOut newReached
      where
        newOut = Set.union out (Map.keysSet reached)
        newReached = Map.fromListWith (++) $ do
          (n, tss) <- Map.toList reached
          (t, n') <- neighbs n
          return (n', map (t:) tss)

shortestWinningStrategies :: Int -> [[Room]]
shortestWinningStrategies n =
  dijkstra (neighbours n) (Set.fromList (allRooms n)) Set.empty

bestWinningStrategy :: Int -> [Room]
bestWinningStrategy = head . shortestWinningStrategies

main :: IO ()
main = forM_ [1..] $ print . shortestWinningStrategies
