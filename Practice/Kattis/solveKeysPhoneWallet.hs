import Data.List ((\\))

main :: IO ()
-- parseInput -> doTheWork -> showResult
main = interact (showResult . doTheWork . parseInput)

parseInput :: String -> [String]
parseInput = tail . lines

-- Takes list of items, xs, and returns list containing whats not in xs from ys
ys = ["phone", "wallet", "keys"]

doTheWork :: [String] -> [String]
doTheWork = (ys \\)

-- Arg type matches the result type of doTheWork
showResult :: [String] -> String
showResult [] = "ready"
showResult xs = unlines xs
