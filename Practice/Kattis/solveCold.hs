type Input = String

type Output = String

type Temperatures = [Int]

type NumFreezing = Int

main :: IO ()
main = interact $ showResult . doTheWork . parseInput

parseInput :: Input -> Temperatures
parseInput = map read . tail . words

doTheWork :: Temperatures -> NumFreezing
doTheWork temps = length $ filter (< 0) temps

showResult :: NumFreezing -> Output
showResult = show

{-
how many days was temp < 0*C

input: 2 lines
a. positive int (1-100) specifying the number of temps
b. n temperatures (-1 000 000-1 000 000)separated by " "

output:
number of temps below 0
-}