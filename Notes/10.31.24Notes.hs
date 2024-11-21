{-
xs = 1 : 2 : 3 : []
-- = [1] -> [2] -> [3] -> *

Make circular:
xs = fix([1, 2, 3] ++)
-- = [1] -> [2] -> [3] -> [1] -> [2] -> [3] -> [1]...
-- fix([1,2,3] ++) -> function that, given a list, appends that list to itself recursively

fix :: (a -> a) -> a
fix f = let x = f x in x
-- = let x = f x in fx where fx = f(fx) = f(f(fx)) = f(f(f(f...)))
-- = let xs = [1, 2, 3] ++ xs in ([1, 2, 3] ++ ([1, 2, 3] ++ xs))
-- -- [1, 2, 3] is all the same list, not a copy
-- [1, 2, 3] ++ xs
-- 1 : ([2,3] ++ xs)
-- 1 : 2 :([3] ++ xs)
-- 1 : 2 : 3 : ([] ++ xs)
-- 1 : 2 : 3 : xs
-}

{-
GHCI stuff:

ghci> import Data.Function (fix)
ghci> xs = fix ([1,2,3]++)
ghci> head xs
1
ghci> head (tail xs)
2
ghci> xs !! 3
1
ghci> take 10 xs
[1,2,3,1,2,3,1,2,3,1]
ghci> data Thing a = Thing a deriving Show
ghci> Thing 5
Thing 5
ghci> Thing 10
Thing 10
ghci> data Thing a = Thing a (Thing a) deriving Show
ghci> thing = let x = Thing 5 y; y = Thing 10 x in (x,y)
ghci> getValue :: Thing a -> a; getValue (Thing n _) = n
ghci> getValue (fst thing)
5
ghci> getValue (snd thing)
10

ghci> :i Monad
(>>=) :: m a -> (a -> m b) -> m b -- Bind operator, called "Then apply"
{-# MINIMAL (>>=) #-}
return :: a -> m a
-- return == pure (they do the same thing)
ghci> [1, 2, 3] ++ return 4
[1, 2, 3] ++ [4]
[1, 2, 3, 4]
ghci> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
ghci> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
ghci> :t (<*>)
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
ghci> -- [].(>>=) :: [a] -> (a -> [b]) -> [b]
ghci> -- IO.(>>=) :: IO a -> (a -> IO b) -> IO b

ghci> ys = ["a\nb\nc", "d\ne\n"]
ghci> :t lines
lines :: String -> [String]
ghci> ys >>= lines -- Going to get :: [String]
["a", "b", "c", "d", "e"]
ghci> [lines] <*> ys
[["a", "b", "c"], ["d", "e"]]
ghci > concat ([lines] <*> ys)
["a", "b", "c", "d", "e"]
ghci > concat (lines <$> ys)
["a", "b", "c", "d", "e"]
ghci > concat (map lines ys)
["a", "b", "c", "d", "e"]
ghci > concatMap (lines ys)

ghci> -- return 5 = [5]
ghci> return' :: a -> [a]; return' = (:[]) -- point free
ghci> return' 5
[5]
ghci> :t return'
return' :: a -> [a]
ghci> return' :: a-> [a]; return' x = [] >>= (x:) -- non-point-free using bind
ghci> return 5
[5]

ghci> :i Monoid -- data type with special properties (must be an empty element, an Identity element)
ghci> import Data.Monoid
ghci> :i sum
type Sum :: * -> *
newtype Sum a = Sum {getSum :: a}
ghci> [1,2,3] `mappend` [4,5,6] -- mappend == (++)
[1,2,3,4,5,6]
-- mempty = []
ghci> [1,2,3] `mappend` mempty
[1,2,3]
ghci> Sum 5 `mappend` Sum 7
Sum {getSum = 12}
ghci> mempty :: Sum Int
Sum {getSum = 0}
ghci> Product 5 `mappend` Product 7
Product {getProduct = 35}
ghci> mempty :: Product Int
Product {getProduct = 1}
ghci> -- mapempty `mappend` x = x
ghci> -- x `mappend` mapempty = x
ghci> -- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
-}
