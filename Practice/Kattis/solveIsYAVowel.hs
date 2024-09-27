type Input = String

type Output = String

type Variables = (Int, Int)

main :: IO ()
main = interact $ showResult . doTheWork . parseInput

parseInput :: Input -> Variables
parseInput = (\[x, y] -> (x, y)) . map read . words

doTheWork :: Variables -> Int
doTheWork (fst, snd)
  | fst > snd = 1
  | otherwise = 0

showResult :: Int -> Output
showResult = show

{-
Given two positive int's, determine if the first > second
input: one line
2 numbers separated by " "
-}