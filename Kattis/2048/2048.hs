import Data.List (transpose)

type Grid = [[Int]]

type Input = String

type Output = String

main :: IO ()
main = interact $ showResult . doTheWork . parseInput

-- Parse the input into a grid and move direction
parseInput :: Input -> (Grid, Int)
parseInput input = (grid, direction)
  where
    ls = lines input
    grid = map (map read . words) (take 4 ls)
    direction = read (ls !! 4)

-- Execute the work to compute the new grid state
doTheWork :: (Grid, Int) -> Grid
doTheWork (grid, direction) = move direction grid
  where
    move 0 = map shiftLeft
    move 1 = transpose . map shiftLeft . transpose
    move 2 = map (reverse . shiftLeft . reverse)
    move 3 = transpose . map (reverse . shiftLeft . reverse) . transpose
    move _ = error "Invalid direction"

-- Shift a row to the left and merge tiles
shiftLeft :: [Int] -> [Int]
shiftLeft row = merged ++ replicate (length row - length merged) 0
  where
    -- Filter non-zero elements, merge adjacent pairs, and re-filter zeros
    merged = merge (filter (/= 0) row)
    merge [] = []
    merge [x] = [x]
    merge (x : y : xs)
      | x == y = (x + y) : merge xs
      | otherwise = x : merge (y : xs)

-- Convert the resulting grid to output format
showResult :: Grid -> Output
showResult = unlines . map (unwords . map show)
