-- sq - Define a square function
sq :: Int -> Int
sq n = n * n

-- sumInts - Sum the numbers between two inclusive values recursively, assuming a < b when the function is first called
sumInts :: Int -> Int -> Int
sumInts f l = sum [f .. l]

-- using recursion
sumInts' :: Int -> Int -> Int
sumInts' f l
  | f == l = f
  | f < l = f + sumInts' (f + 1) l
  | otherwise = error "sumInts': Error, range not valid."

-- sumSquares - Sum the squares between two numbers
sumSquares :: Int -> Int -> Int
sumSquares f l = sum $ map sq [f .. l]

-- higherOrderSum - Define a higher order sum function which accepts an (Int -> Int) function to apply to all integers between two values.
higherOrderSum :: (Num a, Enum a) => (a -> a) -> a -> a -> a
higherOrderSum func f l = sum $ map func [f .. l]

g = higherOrderSum (\x -> x * x + 2 * x - 5)

{-
-- hoSumSquares - Define the square sum in terms of higherOrderSum
hoSumSquares :: (Num a, Enum a) => a -> a -> a
hoSumSquares f l = sum $ map (^ 2) [f .. l]

-- hoSumInts - Define the sum between two values in terms of higherOrderSum
--      Note there is no parameter on the function definition
--      Try writing this in point-free style and/or use a lambda if possible
-- hoSequenceApplication* - Create a new higher order method which generalises over the function provided by sumInts (That is, parameterize (+) :: Int -> Int -> Int) between a and b.
-- hoFactorial* - Define a factorial method using the higherOrderSequenceAppliction.
-}