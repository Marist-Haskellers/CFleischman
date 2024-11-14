# Definitions
reverse :: [a] -> [a]
reverse [] = [] (reverse.1)
reverse (x:xs) = reverse xs ++ [x] (reverse.2)

[] ++ ys = ys
(x:xs) ++ ys = x:(xs ++ ys)

## Prior Results
xs ++ [] = xs (++.rightIdentity)
x ++ (xs ++ ys) = (x ++ xs) ++ ys (++.associativity)

=================================================================================================
# Proof
P(xs): reverse (xs ++ ys) = reverse ys ++ reverse xs

## Base case: P([])
reverse ([] ++ ys)      |       reverse ys ++ reverse []
(++.1) reverse ys       | (reverse.1) reverse ys ++ []
                        | (++.rightIdentity) reverse 

## Inductive case: P(xs) -> P(x:xs)
reverse ((x:xs) ++ ys)                  |     reverse ys ++ reverse(x:xs)
(++.2) reverse x:(xs ++ ys)             | (reverse.2) reverse ys ++ (reverse xs ++ [x])
(reverse.2) reverse (xs ++ ys) ++ [x]   | (++.associativity) (reverse ys ++ reverse xs) ++ [x]
(I.H.) (reverse ys ++ reverse xs) ++ [x]|
=================================================================================================

=================================================================================================
# Definition
(f . g) x = f (g x) (funcComp.1)
head [] = undefined (head.0)
head (x:_) = x (head.1)
map _ [] = [] (map.1)
map f (x:xs) f x : map f xs (map.2)

# Proof
P: head . map f = f . head
   (head . map f) xs = (f . head) xs -- Rewrite un-point-free

## Base case 1: P(undefined)
(head . map f) undefined             | (f. head) undefined
(funcComp.1) head (map f undefined)  | (funcComp.1) f (head undefined)
(if 'f' is strict) head undefined    | (head is strict) f  undefined
(head is strict) undefined           | (if 'f' is strict) undefined

## Base case 2: P([])
(head . map f) []             | (f. head) []
(funcComp.1) head (map f [])  | (funcComp.1) f (head [])
(map.1) head ([])             | (head.0) f (undefined)
(head.0) undefined            | (if 'f' is strict) undefined

## Case 3: P(xs) -> P(x:xs)
(head . map f) (x:xs) | (f . head) (x:xs)
head (map f (x:xs))   | f (head (x:xs))
head (f x : map f xs) | f x
f x
=================================================================================================

=================================================================================================
# Proof

sqr x = x * x

# Eager evaluation
sqr (sqr (3 + 4))
   = sqr (sqr (7))
   = sqr (7 * 7)
   = sqr 49
   = (49 * 49)
   = 2401
# Lazy evaluation
sqr (sqr (3 + 4))
   = let x = sqr (3 + 4) in x * x
   = let y = (3 + 4) in let x = y * y in x * x
-- looks like:
   = ((3+4) * (3+4)) * ((3+4) * (3+4))
-- What is done:
   = let x = sqr (3 + 4)
      in x * x
   = let y = (3 + 4)
      in let x = y * y
         in x * x
   = let y = 7
      in let x = y * y
         in x * x
   = let x = 49
      in x * x
   = 2401

=================================================================================================
=================================================================================================
f :: a -> b -> a
f x y = x

# Lazy
f 5 (sqr (sqr (3 + 4)))
   = let x = 5
      y = sqr (sqr (3 + 4))
      in x
   = 5

# Eager  
-- note how many '=' per solve, 
-- each '=' is each step/process with the size of
-- the statement being how large it is in memory
f 5 (sqr (sqr (3 + 4)))
   = f 5 (sqr (sqr (7)))
   = f 5 (sqr (49))
   = f 5 2401
   = 5

=================================================================================================
=================================================================================================
head (map (*10) ([1,2,3] ++ [4..]))
   = let (x:_) = map (*10) ([1,2,3] ++ [4..])
      in x
   = let f = (*10)
      (y : ys) = [1, 2, 3] ++ [4 ..]
      in let (x:_) = f y : map f ys
         in x
   = let (z:zs) = 1 : ([2,3] ++ [4..])
      in let f = (*10)
         (y : ys) = z : zs
         in let (x:_) = f y : map f ys
            in x
   = ... -- small steps to get to 10, just solve from inside out
   = 10