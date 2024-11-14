type Input = String
type Output = String

main ::IO()
main = interact $ showResult.doTheWork.parseInput

parseInput ::Input -> _
parseInput = map read.___.words

doTheWork :: _ - > _
doTheWork = undefined

showResult :: _ -> Output
showResult = show ____


{-
Haskell source file, preferably named after the problem. 
Tip: just use the problem identifier from the Web page URL. 
For example, the problem "A1 Paper" is described on the page https://open.kattis.com/problems/a1paper, 
so name I would name the file as a1paper.hs.
-}