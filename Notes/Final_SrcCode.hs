-- Monoids Class Definition
class Monoid a where
  mempty :: a -- Identity element
  mappend :: a -> a -> a -- Binary operation
  mconcat :: [a] -> a -- Fold operation for a list
  (<>) :: a -> a -> a -- Infix version of mappend
  x <> y = mappend x y -- Default implementation

-- Monoids Examples

-- Lists
listExample :: [Int]
listExample = [1, 2] <> [3, 4] -- Result: [1, 2, 3, 4]

-- Sum Monoid
newtype Sum a = Sum {getSum :: a}

instance (Num a) => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum (x + y)

-- Product Monoid
newtype Product a = Product {getProduct :: a}

instance (Num a) => Monoid (Product a) where
  mempty = Product 1
  mappend (Product x) (Product y) = Product (x * y)

-- Maybe Monoid
instance (Monoid a) => Monoid (Maybe a) where
  mempty = Nothing
  mappend Nothing y = y
  mappend x Nothing = x
  mappend (Just x) (Just y) = Just (x <> y)

-- Monad Class Definition
class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b -- Bind operator
  return :: a -> m a -- Wraps a value in the monad
  (>>) :: m a -> m b -> m b -- Sequential execution

-- Monad Examples

-- Maybe Monad
maybeExample :: Maybe Int
maybeExample = Just 5 >>= (\x -> Just (x + 1)) -- Result: Just 6

-- List Monad
listMonadExample :: [Int]
listMonadExample = [1, 2] >>= (\x -> [x, x * 2]) -- Result: [1, 2, 2, 4]

-- IO Monad Example
ioExample :: IO ()
ioExample = do
  putStrLn "Enter a number:"
  num <- readLn
  print (num * 2)

-- Zipper Definitions

-- List Zipper
type ListZipper a = ([a], a, [a]) -- (left context, focus, right context)

moveRight :: ListZipper a -> ListZipper a
moveRight (ls, focus, r : rs) = (focus : ls, r, rs)

modifyFocus :: (a -> a) -> ListZipper a -> ListZipper a
modifyFocus f (ls, focus, rs) = (ls, f focus, rs)

-- Tree Zipper
data Tree a = Leaf a | Node a (Tree a) (Tree a)

type Breadcrumb a = (a, Tree a, Tree a)

type TreeZipper a = (Tree a, [Breadcrumb a])

-- Lazy Evaluation Examples

-- NF and WHNF
nfExample :: Int
nfExample = 1 + 2 -- Fully evaluated: 3

whnfExample :: Int -> Int
whnfExample x = x + 2 -- Partially evaluated until `x` is known
