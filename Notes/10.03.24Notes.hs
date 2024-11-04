-- Explain mapping, functors, maybe, either, map/fmap..

-- lift: when a function is mapped over data. i.e. map f over [a] to get [b] is lifting f

{-
Type:    Kind:
[]    * -> *
(_,_) * -> * -> *

fmap (*2) (True, 7) = (True, 14)

Functor:
      fmap f over (t, a) -> (t, b)
   Maybe
      fmap f over Maybe a -> Maybe b

fmap (*2) Nothing = Nothing

fmap = map -- for lists
-}
-- Lists can represent computations with multiple solutions or zero solutions
-- Maybe functor can represent computations that may succeed or fail

{-
fmap (*2) x
   is the same as
(*2) <$> x

Either type * -> * -> *
fmap f over Either t a -> Either t b

Functor:

class Functor f where
   fmap :: (a -> b) -> (f a -> f b)

instance Functor BinaryTree where
   fmap ...

fmap (f(g(x))) = fmap f . fmap g (x)
-}
