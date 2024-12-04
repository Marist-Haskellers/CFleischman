import Data.List (nub)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set

type Input = String

type Output = String

main :: IO ()
main = interact $ showResult . doTheWork . parseInput

parseInput :: Input -> (Int, Int, [(Int, Int)])
parseInput input = (n, m, edges)
  where
    (header : rest) = lines input
    [n, m] = map read (words header)
    edges = map ((\[x, y] -> (x, y)) . map read . words) rest

doTheWork :: (Int, Int, [(Int, Int)]) -> Int
doTheWork (n, m, edges) = connectedComponentsCount - 1
  where
    adjacencyMap = buildAdjacencyMap edges
    connectedComponentsCount = length $ findConnectedComponents n adjacencyMap

buildAdjacencyMap :: [(Int, Int)] -> Map.Map Int [Int]
buildAdjacencyMap = foldr insertEdge Map.empty
  where
    insertEdge (u, v) acc =
      Map.insertWith (++) u [v] $ Map.insertWith (++) v [u] acc

findConnectedComponents :: Int -> Map.Map Int [Int] -> [[Int]]
findConnectedComponents n adjMap = go [1 .. n] Set.empty []
  where
    go [] _ components = components
    go (x : xs) visited components
      | x `Set.member` visited = go xs visited components
      | otherwise =
          let component = dfs x Set.empty
           in go xs (visited `Set.union` Set.fromList component) (component : components)
    dfs node visited
      | node `Set.member` visited = []
      | otherwise = node : concatMap (`dfs` Set.insert node visited) neighbors
      where
        neighbors = fromMaybe [] (Map.lookup node adjMap)

showResult :: Int -> Output
showResult = show
