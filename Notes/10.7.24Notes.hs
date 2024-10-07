-- Beginning of proofs
--    Lots of proofs on exam 2 and 3

{-
Haskell:
   "pure" functional language -> referential transparency
         referential transparency => if f(x) = y one time, then f(x) is always equal to y
      no computational side-effects (i.e. changing memory location data, or I.O. (read/write) ability)
   *Guaranteed results

WHNF - Weak Head Normal Form:
   E.g. data [a] = a : [a] | []
                  ^^^^^^^^   ^^  <- these are data constructors
   > data List a = Cons a (List a) | Null

   An expression that is ...
      * a data constructor applied to its arguments; or
      * a lambda expression; or
      * a built-in function applied to some, but not all, of its arguments

   head :: [a] -> a
   head (x : xs) = x
   > head [1,2,3]    -- not in WHNF
   > = head (1:(2:(3:[])))
   > = 1    -- in WHNF

   (++) - Append lists
   [] ++ ys = ys
   (x:xs) ++ ys = x:(xs ++ ys)   -- recursive definition
   > [1,2,3] ++ [4..]
   > 1:[2,3] ++ [4..]   -- moved first term to WHNF (in WHNF because of ':' operator)
   > 1:([2,3] ++ [4..])    -- whole thing is in WHNF now
      ^  ^^^^^^^^^^^^ <- Note 1 is the head and (___) is the tail, separated by :

   "Red-ex" - reducible expression:
      can be reduced to a single, concrete value (Normal Form)

   Proof by Induction:
      Some proposition 'P' to be proved
      Prove P(0) holds true   -- prove 'P' for some number '0' (Base case)
      Prove implication P(n) -> P(n+1)    -- if P(n) is True then P(n+1) is True (Inductive case)

   Prove: "reverse(reverse(xs)) = xs"
      P([]) holds true and  [Yes]
      P(xs) -> P(x : xs)    [Yes]

   Assumptions:
      (xs ++ ys) ++ zs = xs ++ (ys ++ zs)
   Def.
      reverse 1. reverse [] = []
      reverse 2. reverse (x:xs) = reverse (xs) ++ [x]
      ++ 1. [] ++ ys = ys
      ++ 2. (x:xs) ++ ys = x:(xs ++ ys)

   reverse(reverse([]))     |     []
   -------------------------------------------------
   reverse[] // reverse 1.  | reverse[]  // reverse 1.
   -------------------------------------------------
   reverse[] == reverse[] so P[] holds true

   reverse(reverse(x : xs))                        |              (x :xs)
   --------------------------------------------------------------------------------------------------
   reverse (reverse xs ++ x) // reverse 2.         |
   x : reverse (reverse xs) // intermediate result |
   xs ++ [x] // inductive hypothesis               |
   (x:xs)
   --------------------------------------------------------------------------------------------------
   (x:xs) == (x:xs) so the inductive case holds true

   inductive hypothesis: if we reverse(reverse(xs)) = xs
   See Notebook for written proofs on how reverse(ys++[x]) = x:reverse ys *Intermediate result

   Since P([]) is true and P(xs) -> P(x:xs) is true, then P is proven

-}
