type Input = String

type Output = String

type Minute = Int

type Hour = Int

type Time = (Hour, Minute)

type Problem = Time

type Solution = Time

main :: IO ()
main = interact $ showResult . doTheWork . parseInput

parseInput :: Input -> Problem
parseInput = undefined

doTheWork :: Problem -> Solution
doTheWork = undefined

showResult :: Solution -> Output
showResult = undefined