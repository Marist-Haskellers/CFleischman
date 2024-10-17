type Input = String

type Output = String

type TestCase = (String, String)

type Result = (TestCase, String)

main :: IO ()
main = interact $ showResult . doTheWork . parseInput

parseInput :: Input -> [TestCase]
parseInput = readCases . tail . lines
  where
    readCases [] = []
    readCases (s1 : s2 : ss) = (s1, s2) : readCases ss

doTheWork :: [TestCase] -> [Result]
doTheWork = map solve1
  where
    solve1 :: TestCase -> Result
    solve1 cs = (cs, uncurry (zipWith diff1) cs)
    diff1 :: Char -> Char -> Char
    diff1 c1 c2 = if c1 == c2 then '.' else '*'

showResult :: [Result] -> Output
showResult = unlines . concatMap report1
  where
    report1 :: Result -> [String]
    report1 ((s1, s2), d) = [s1, s2, d]

-- report1 r = [fst (fst r), snd (fst r), snd r]