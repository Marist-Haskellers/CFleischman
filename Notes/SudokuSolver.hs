import Data.List (nub)

-- Sudoku solver
-- type synonyms
type Matrix a = [Row a]

type Row a = [a]

type Grid = Matrix Digit

type Digit = Char

digits :: [Char]
digits = ['1' .. '9']

blank :: Char -> Bool
blank = (== 'X')

-- brute force solving
solve :: Grid -> [Grid]
solve = filter valid . completions

valid :: Grid -> Bool
valid g = all noDuplicates (rows g) && all noDuplicates (cols g) && all noDuplicates (boxes g)

noDuplicates :: [Digit] -> Bool
noDuplicates [] = True
noDuplicates (x : xs) = x `notElem` xs && noDuplicates xs

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols [xs] = [[x] | x <- xs]
cols (xs : xss) = zipWith (:) xs (cols xss)

boxes :: Matrix a -> [Row a]
boxes = map ungrouped . ungrouped . map cols . group . map group

ungrouped = concat

group [] = []
group (x : y : z : xs) = [x, y, z] : group xs

completions :: Grid -> [Grid]
completions = expand . prune . choices

type Choices = [Digit]

choices :: Grid -> Matrix Choices
-- choices = let choice d = if blank d then digits else [d] in map $ map choice
choices = map $ map choice
  where
    choice d
      | blank d = digits
      | otherwise = [d]

expand :: Matrix Choices -> [Grid]
expand = cp . map cp

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs : xss) = [x : ys | x <- xs, ys <- cp xss]

-- pruning, making it more efficient
prune :: Matrix Choices -> Matrix Choices
prune =
  pruneBy boxes . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . map pruneRow . f

pruneRow :: Row Choices -> Row Choices
pruneRow row = map (remove ones) row
  where
    ones = [d | [d] <- row]

remove :: Choices -> Choices -> Choices
remove xs [d] = [d]
remove xs ds = filter (`notElem` xs) ds

-- single cell expansion
expandCell :: Matrix Choices -> [Matrix Choices]
expandCell rows =
  [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where
    (rows1, row : rows2) = break (any smallest) rows
    (row1, cs : row2) = break smallest row
    smallest cs = length cs == n
    n = minimum (counts rows)

counts = filter (/= 1) . map length . concat

solve2 :: Grid -> [Grid]
solve2 = search . choices

search :: Matrix Choices -> [Grid]
search cm
  | not (safe pm) = []
  | complete pm = [ma [] (map head) pm]
  | otherwise = (concat . map search . expandCell)

{- In ghci:
:l SudokuSolver.hs
g = ["14X", "8X9", "67X"]
choices g
[["1","4","123456789"],["8","123456789","9"],["6","7","123456789"]]
expand (choices g)
-- SHOWS EVERY COMBO OF CHOICES -- -}