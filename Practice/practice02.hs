-- compute the result of a univariate cubic polynomial given the coefficients and the value of the variable

-- cubic :: Int -> Int -> Int -> Int -> Int -> Int
cubic :: (Num a) => a -> a -> a -> a -> a -> a
cubic a b c d x = a * x ^ 3 + b * x ^ 2 + c * x + d

-- write a function that computes the result of any univariate polynomial function given the coefficients as a list and the value of the variable
polynomial :: [Int] -> Int -> Int
-- takes the sum of x^y, where y = [0...], and the list ys
polynomial ys x = sum (zipWith (*) ys (map (x ^) [0 ..]))

-- get the last element of a non-empty list without using the Prelude function last
lastElem :: [a] -> a
-- lastElem [] = error "lastElem: empty list"
lastElem = head . reverse

-- implement the logic of the Prelude map function without using that function
applyToEach :: (a -> b) -> [a] -> [b]
applyToEach _ [] = []
applyToEach f (x : xs) = f x : applyToEach f xs