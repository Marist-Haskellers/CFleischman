import Language.Haskell.TH (safe)

-- parseSolveAndReport :: IO ()
-- parseSolveAndReport = do
--       s1 <- getLine
--       s2 <- getLine
--       putStrLn s1
--       putStrLn s2
--       let sd = do
--          (c1, c2) <- zip s1 s2
--          return $ if c1 == c2 then '.' else '*'
--       putStrLn sd
--       putStrLn ""

{-

ghci> i: Functor
type Functor :: (* -> *) -> Constraint
class Functor f where
   fmap :: (a -> b) -> f a -> f b
   (<$) :: a -> f b -> f a
   {-# MINIMAL fmap #-}
   -- Defined in `GHC.Base'

instance Functor ((,) a) -- Defined in `GHC.Base'
instance Functor ((,,) a b) -- Defined in `GHC.Base'
instance Functor ((,,,) a b c) -- Defined in `GHC.Base'
instance Functor ((->) r) -- Defined in `GHC.Base'
instance Functor IO -- Defined in `GHC.Base'
instance Functor Maybe -- Defined in `GHC.Base'
instance Functor Solo -- Defined in `GHC.Base'
instance Functor [] -- Defined in `GHC.Base'
instance Functor (Either a) -- Defined in `Data.Either'

ghci> i: (<$>)
(<$>) :: (Functor f) => (a -> b) -> f a -> f b

ghci> :i Applicative
type Applicative :: (* -> *) -> Constraint
class Functor f => Applicative f where
   pure :: a -> f a                    -- Wraps value in wanted type (> pure 5 :: Maybe Int; > Just 5)
   (<*>) :: f (a -> b) -> f a -> f b
   GHC.Base.liftA2 :: (a -> b -> c) -> f a -> f b -> f c
   (*>) :: f a -> f b -> f b
   (<*) :: f a -> f b -> f a
   {-# MINIMAL pure, ((<*>) | liftA2) #-}
         -- Defined in `GHC.Base'
instance Monoid a => Applicative ((,) a) -- Defined in `GHC.Base'
instance (Monoid a, Monoid b) => Applicative ((,,) a b)
   -- Defined in `GHC.Base'
instance (Monoid a, Monoid b, Monoid c) =>
         Applicative ((,,,) a b c)
   -- Defined in `GHC.Base'
instance Applicative ((->) r) -- Defined in `GHC.Base'
instance Applicative IO -- Defined in `GHC.Base'
instance Applicative Maybe -- Defined in `GHC.Base'
instance Applicative Solo -- Defined in `GHC.Base'
instance Applicative [] -- Defined in `GHC.Base'
instance Applicative (Either e) -- Defined in `Data.Either'

(<*>) :: [ ([a] -> Int) ] -> ([[a]] -> [Int])

ghci> [length] <*> ["apple", "banana", "cherry", "date"]
   > [5, 6, 6, 4]

ghci> countVowels = length . filter (`elem` "aeiou")
ghci> :t countVowels
   > countVowels :: [Char] -> Int

ghci> [countVowels] <*> ["apple", "banana", "cherry", "date"]
   > [2, 3, 1, 2]
ghci> [length, countVowels] <*> ["apple", "banana", "cherry", "date"]
   > [5, 6, 6, 4, 2, 3, 1, 2]

ghci> fmap f a = f <$> a = pure f <*> a

Functor Laws:
   Preservation of Identity:
      fmap id x = id x
   Preservation of Composition:
      fmap (f.g) x = (fmap f . fmap g) x

Applicative Functor Laws:
   (1) Identity Law (preserves func identity)
      pure id <*> x = x
   (2) Homomorphism Law (preserves func application)
      pure (g x) = pure g <*> pure x
   (3) Interchange Law (order swaping)
      u <*> pure y = pure ($ y) <*> us
   (4) Composition Law
      ((pure (.) <*> u) <*> v) <*> w = u <*> (v <*> w)

((pure (.) <*> [f, g]) <*> [h]) <*> [x, y, z]
   = ([(f.), (g.)] <*> [h]) <*> [x, y, z]
   = [f.h, g.h] <*> [x, y, z]
   = [(f.h) x, (f.h) y, (f.h) z, (g.h) x, (g.h) y, (g.h) z]
-- same as
   = [f(h x), f(h y), f(h z), g(h x), g(h y), g(h z)]
-}

-- See NOTEBOOK --