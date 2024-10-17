-- ghci> :{
-- ghci| f :: IO()
-- ghci| f = do
-- ghci|   x <- readLn
-- ghci|   y <- readLn
-- ghci|   print $ x * y
-- ghci| :}
-- ghci> f
-- 5
-- 10
-- 50

-- ghci> g = readLn >>= (\x -> (readLn >>= (\y -> print (x * y))))
-- ghci> g
-- 10
-- 5
-- 50

-- ghci> :{
-- ghci| g = readLn >>= (\x ->
-- ghci|      readLn >>= (\y ->
-- ghci|       print $ x * y))
-- ghci| :}
-- ghci> g
-- 25
-- 2
-- 50

-- ghci > xs = [1, 2, 3] >>= (\x -> [x * 2])
-- ghci > xs
-- [2, 4, 6]

-- ghci> :{
-- ghci| ys = do
-- ghci|   y <- [1,2,3]
-- ghci|   [y * 2]
-- ghci| :}
-- ghci> ys
-- [2,4,6]

{-
curry
uncurry

flip opperator

-}