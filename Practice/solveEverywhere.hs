import Data.List (nub)

main :: IO ()
main = interact (showResult . doTheWork . parseInput . lines)

parseInput :: [String] -> ([Int], [[String]])
parseInput (x : xs) = (map read [x], splitTrips (map read [x]) xs)
  where
    splitTrips [] _ = []
    splitTrips (y : ys) zs = take y zs : splitTrips ys (drop y zs)

doTheWork :: ([Int], [[String]]) -> [Int]
doTheWork (_, trips) = map (length . nub) trips

showResult :: [Int] -> String
showResult = unlines . map show
