import Data.List (elemIndices)

{- average - compute the average of a list xs of numbers (or zero if the list is empty).
Example: average [5, 10, 15, 20] = 12.5. -}

average :: (Integral a) => [a] -> a
average [] = 0
average (x : xs)
  | null xs = error "average: empty list"
  | otherwise = fromIntegral $ (x + average xs) `div` 2

{- duple - given an integer n and a value x return a list containing n copies of x.
Example: duple 4 2 = [2,2,2,2].
Example: duple 5 'A' = "AAAAA".
Example: duple 3 [1,2] = [[1,2],[1,2],[1,2]]". -}

duple :: Int -> a -> [a]
duple = replicate

{- down - wraps each top level element of a list xs in a list.
Example: down [1,2,3] = [[1],[2],[3]].
Example: down ["a","fine","idea"] = [["a"],["fine"],["idea"]].
Example: down [[True],[False],[False],[True]] = [[[True]],[[False]],[[False]],[[True]]]. -}

down :: [a] -> [[a]]
down (x : xs) = [x] : down xs

{- swapper - takes two values y1 and y2 and a list xs and returns a list the same as xs but with all occurrences of y1 replaced by y2 and all occurrences of y2 replaced by y1.
Example: swapper 'a' 'd'  "abcd" = "dbca".
Example: swapper "a" "d"  ["a","d","","c","d"] = ["d","a","","c","a"].
Example: swapper 1 0  [1,0,0,1] = [0,1,1,0]. -}

swapper :: (Eq a) => a -> p -> [a] -> [Int]
swapper y1 y2 xs =
  if null xs then error "swapper: empty list" else y1 `elemIndices` xs
