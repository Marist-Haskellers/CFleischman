-- The Amazing Human Cannonball

-- Kattis problem https://open.kattis.com/problems/humancannonball2

type Input = String

type Output = String

main :: IO ()
main = interact $ showResult . doTheWork . parseInput

parseInput :: Input -> [[Double]]
parseInput input = map (map read . words) (tail (lines input))

doTheWork :: [[Double]] -> [String]
doTheWork =
  map
    ( \[velocity, angle, distance, heightLow, heightUp] ->
        let gravity = 9.81
            theta = angle * pi / 180
            time = distance / (velocity * cos theta)
            position = velocity * time * sin theta - 0.5 * gravity * time ^ 2
         in if position > heightLow + 1 && position < heightUp - 1
              then "Safe"
              else "Not Safe"
    )

showResult :: [String] -> Output
showResult = unlines
