type Input = String

type Suffix = String

type Output = String

main :: IO ()
main = interact $ showResult . doTheWork . parseInput

parseInput :: Input -> String
parseInput = id

doTheWork :: String -> Suffix
doTheWork = dropWhile (/= 'a')

showResult :: Suffix -> Output
showResult = id

{-
given a string s, guaranteed to to have the char a
output the tail of s whose head is a
-}