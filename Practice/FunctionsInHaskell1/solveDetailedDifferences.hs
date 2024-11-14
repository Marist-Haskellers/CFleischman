type Input = String

type Output = String

main :: IO ()
main = interact $ showResult . doTheWork . parseInput

parseInput :: Input -> [[String]]
parseInput = pairs . tail . lines
  where
    pairs [] = []
    pairs (x : y : xs) = [x, y] : pairs xs

doTheWork :: [[String]] -> [String]
doTheWork = concatMap (\[a1, a2] -> [a1, a2, zipWith compareChars a1 a2])
  where
    compareChars x y = if x == y then '.' else '*'

showResult :: [String] -> Output
showResult = unlines

{-
write a program which identifies the differences between pairs of strings to make it easier for humans to see the differences
input:
fst line: # of test cases
a1: compare w/ a2
a2: compare against a1
b1: ""
b2: ""
c1:""
...

output:
a1
a2
a*: sequences of * and . where * denotes a difference between a1 and a2
-}