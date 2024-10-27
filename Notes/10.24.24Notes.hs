#Definitions
(tail.1) tail (_:xs) = xs
(take.1) take _ [] = []
(take.2) take 0 _ = []
(take.3) take n (x:xs) = x : take (n-1) xs

--- Solve

take 2 (tail [1,2,3,4])
-- must be in Head Normal Form
   => let xs = tail [1,2,3,4] -- insert let for unreduced sub-expression
      in take 2 xs

   => let xs = [2,3,4] -- [1,2,3,4] is in WHNF and tail def. makes [2,3,4]
      in take 2 xs

   => 2 : take (2-1) [3,4] -- reduce whole expression

   => let n = 2-1 -- insert let for unreduced sub-expression
      in 2 : take n [3,4]

   => let n = 1 -- reduce sub-expression
      in 2 : take n [3,4]

   => 2 : take 1 [3,4] -> 2 : (3 : take (1-1) [4]) -- reduce whole expression

   => let n = 1-1 -- insert let for unreduced sub-expression
      in 2 : (3 : take (1-1) [4])

   => let n = 0 -- reduce sub-expression
      in 2 : (3 : take 0 [4])

   => 2 : (3 : take 0 [4]) -> 2 : (3 : []) -> [2,3] -- reduce whole expression to normal form


--- Solve


#Definition
(map.1) map _ [] = []
(map.2) map f (x:xs) = f x : map f xs
(head.1) head (x:_) = x

head (map (*10) ([1,2,3] ++ [4..]))
   => let xs = map (*10) ([1,2,3] ++ [4..]) -- insert let for sub-expression
      in head xs

   => let ys = [1,2,3] ++ [4..]
      in let xs = map (*10) ys --insert let for sub-expression
         in head xs

   => let ys = 1 : ([2,3] ++ [4..]) -- applied def of ++
      in let xs = map (*10) ys
      in head xs

   => let xs = (*10) 1 : map (*10) ([2,3] ++ [4..]) -- plugging in ys and def map.2
      in head xs

   => 1 * 10 -- plug in xs and apply head.1

   => 10 -- solve