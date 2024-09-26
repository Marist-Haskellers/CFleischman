map' :: (a -> b) -> [a] -> [b]
map' = undefined

filter' :: (a -> bool) -> [a] -> [b]
filter' = undefined

zip' :: [a] -> [b] -> [(a, b)]
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys -- ',' pair constructor, how we make pairs
zip' [] ys = []
zip' xs [] = []

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

sumSquares :: (Num a, Enum a) => a -> a -> a
sumSquares fst lst = sum $ map' (^ 2) [fst .. lst]

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z xs
  | null xs = z
  | otherwise = f (head xs) (foldr' f z (tail xs))

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z xs
  | null xs = z
  | otherwise = foldl' f (f z (head xs)) (tail xs) -- where "(f z (head xs))" is the new z value

{-  -- Folding list down to one value using "+" operator
Folding [a] -> [b]
(a->a->a) -> [a] -> a

fold [1,2,3,4,5] == ((((0+1)+2)+3)+4)+5 : [] <- left fold
fold [1,2,3,4,5] == 1+(2+(3+(4+(5+0)))) : [] <- right fold

1+2 = 3
3+3 = 6
6+4 = 10
10+5 = 15

sum [1,2,3,4,5]
0+1 = 1
1+2 = 3
3+3 = 6
6+4 = 10
10+5 = 15

-- If the operator is equivalent i.e + or * then foldl == foldr

-}

-- cycle [1,2,3] = [1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2..]

factorial' :: Int -> Int -- Explicit recursion
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

factorial'' :: Int -> Int -- Using fold
factorial'' 0 = 1
factorial'' n = foldl' (*) 1 [1 .. n]