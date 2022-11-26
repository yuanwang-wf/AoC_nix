-- |

module Y2021.Day12 where
import Data.Char (isLower)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set


type Node = String

type Edge = (Node, Node)

type Graph = Map Node (Set.Set Node)

isSmallCave :: Node -> Bool
isSmallCave = all isLower


buildGraph :: [Edge] -> Map Node (Set.Set Node)
buildGraph = foldr buildEdge Map.empty

buildEdge :: Edge -> Map Node (Set.Set Node) -> Map Node (Set.Set Node)
buildEdge (a, b)  = Map.insertWith Set.union b (Set.singleton a)  . Map.insertWith Set.union a (Set.singleton b)

testData :: [Edge]
testData = [("start", "A"), ("start", "b"), ("A", "c"), ("A", "b"), ("b", "d"), ("A", "end"), ("b", "end")]

-- findPath :: Graph
-- findPath = _
