type Input = String

type Output = String

main :: IO ()
main = interact $ showResult . doTheWork . parseInput

-- "4\n1 0 5\n"
parseInput :: Input -> [Integer]
parseInput = map read . tail . words

-- From a list of counts of papers at each size, how much tape needed, if any?
doTheWork :: [Integer] -> Maybe Double
doTheWork = fmap (tapeLength . levelUp) . papersToUse

showResult :: Maybe Double -> Output
showResult (Just x) = show x
showResult Nothing = "impossible"

-- From list of papers we have, find how many papers
-- we will use of size, or if we can't make it
-- 2 A2 = A1
-- 2 A3 = 1 A2
-- 2 A4 = 1 A3
papersToUse :: [Integer] -> Maybe [Integer]
papersToUse = papersHelper 2
  where
    -- where y is what you need and x:xs is list of what you have
    papersHelper _ [] = Nothing
    papersHelper y (x : xs)
      | x >= y = Just [y]
      | otherwise = (x :) <$> papersHelper (2 * (y - x)) xs

-- papersHelper _ [] = Nothing
-- papersHelper (Just (y : ys)) (x : xs)
--    | x >= y = Just [y]
--    | otherwise = (x:) <$> papersHelper (Just ys) xs

-- How many long edges at each size need to be taped
levelUp :: [Integer] -> [Integer]
levelUp [x] = [x `div` 2]
levelUp (x : xs) = (x + y) `div` 2 : y : ys
  where
    (y : ys) = levelUp xs

-- From the edge counts at each size, find total length of tape needed
tapeLength :: [Integer] -> Double
tapeLength edgeCounts = sum $ zipWith ((*) . fromIntegral) edgeCounts edgeLengths

-- List of lengths for all long edges at each paper size
-- A2 is 2**(-3/4), A3 is 2**(-5/4), A4 is 2**(-7/4)..
-- Ai is 2**(-(2*i-1)/4)
edgeLengths :: [Double]
edgeLengths = [2 ** (-(k / 4)) | k <- [3, 5 ..]] -- List comprehension
