{- Proofs
Sum.1 sum [] = 0
Sum.2 sum (x:xs) = x + sum xs
Prove: sum(xs ++ ys) = sum xs + sum ys
Append.1 [] ++ ys = ys
Append.2 (x:xs) ++ ys = x:(xs ++ ys)
---------------------------------
sum(xs ++ ys) | sum xs + sum ys
---------------------------------
-- Base case P[]: prove that sum (xs ++ ys) = sum xs + sum ys where sum [] = 0 //Assume xs = []
sum([] ++ ys)    | sum [] + sum ys
[] ++ ys         | 0 + sum ys
sum [] + sum ys  | ""
0 + sum ys       | 0 + sum ys
sum ys           = sum ys

I.H. - P(xs) -> P(x:xs) - {Inductive hypothesis}
-- Inductive step, given P(xs) -> P(x:xs): prove that sum (xs ++ ys) = sum xs + sum ys where sum (x:xs) = x + sum xs //Assume xs = (x:xs)
sum((x:xs) ++ ys)  |
sum((x:(xs ++ ys)) | sum (x:xs) + sum ys
                   | x + sum xs + sum ys
x + sum xs + sum ys = x + sum xs + sum ysa

1. concat [] = []
2. concat (xs:xss) = xs ++ concat xss
Q. concat (xss ++ yss) = concat xss ++ concat yss
---------------------------------
concat (xss ++ yss) | concat xss ++ concat yss
---------------------------------
\|
\|
\|

1. filter p [] = []
2. filter p (x:xs) = if p x then x:filter p xs else filter p xs
Q. filter p (xs ++ ys) = filter p xs ++ filter p ys
---------------------------------
filter p (xs ++ ys) | filter p xs ++ filter p ys
---------------------------------
\|
\|
\|

-}