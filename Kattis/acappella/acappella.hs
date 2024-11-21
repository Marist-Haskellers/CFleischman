-- Kattis problem https://open.kattis.com/problems/acappellarecording

import Data.List (sort)

type Input = String

type Output = String

main :: IO ()
main = interact $ showResult . doTheWork . parseInput

parseInput :: Input -> (Int, Int, [Int])
parseInput input = (notes, difference, pitches)
  where
    (x : xs) = lines input
    [notes, difference] = map read (words x)
    pitches = map read xs

doTheWork :: (Int, Int, [Int]) -> Int
doTheWork (notes, difference, pitches) = partitionPitches (sort pitches) difference
  where
    partitionPitches :: [Int] -> Int -> Int
    partitionPitches [] _ = 0
    partitionPitches (p : ps) diff = 1 + partitionPitches remaining diff
      where
        remaining = dropWhile isWithinDifference ps
        isWithinDifference x = x - p <= diff

showResult :: Int -> Output
showResult = show

{-
Testing in ghci:
  ghci> doTheWork (5, 3, [1, 2]) -- expected num pitches > actual num pitches
  1 -- Correct
  ghci> doTheWork (4, 100, [1, 50, 100, 150]) -- diff too large
  2 -- Correct
  ghci> doTheWork (4, 0, [1, 2, 3, 4]) -- diff too small
  4 -- Correct
  ghci> doTheWork (4, 5, [10, 10, 10, 10]) -- all pitches are identical
  1 -- Correct
-}