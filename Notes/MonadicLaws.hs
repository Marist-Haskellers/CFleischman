{-
Monoid Laws:
mempty <> u = u | Left Identity
u <> mempty = u | Right Identity
(u <> v) <> w = u <> (v <> w) | Associativity

Monad Laws:
   return x >>= f = f x | Left Unit
   m >>= return = m | Right Unit
   (m >>= f) >>= g = m >>= (\x -> f x >>= g) | Associativity

Monoid m :: *
   mempty :: m
   mappend or <> :: m -> m -> m
   mconcat :: [m] -> m

Monad m :: * -> *
   return :: a -> m a
   (>>=) :: m a -> (a -> m b) -> m b | Bind
   (>>) :: m a -> m b -> m b
   fail :: String -> m a

Misc.
   [] :: * -> *
   [Int] :: *
   IO String :: *

instance Monoid <type_name> where
   mempty = Hearts
   mappend x y = Diamonds
   mconcat = foldr mappend mempty   -- Does not need to be defined
-}