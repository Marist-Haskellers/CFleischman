import Data.List (nub)

main :: IO ()
main = interact $ showResult . doTheWork . parseInput

parseInput :: String -> [String]
parseInput = tail . lines

doTheWork :: [String] -> [Int]
doTheWork = map (length . nub) . splitTrips
  where
    splitTrips [] = []
    splitTrips (x : xs) = take (read x) xs : splitTrips (drop (read x) xs)

showResult :: [Int] -> String
showResult = unlines . map show
