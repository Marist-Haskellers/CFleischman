{-
Normal Form (NF)
   cannot be reduced further

-- Goes to NF if final result is needed, else only stays in WHNF
Weak Head Normal Form (WHNF)
   reduced to a data constructor
   i.e. data [a] = [] | (:) a [a] --> a : [a]
      map f xs : map (map g) ys -- In WHNF

(map ((++ "px").show) [30, 45, 60, 75]) !! 2
                     30:(45:(60:(75:([]))))
-- Look at outer most operation and go in from there if not WHNF, convert then go back out
!! 2 -- Not WHNF
{
Let g = ((++ "px").show) in ((g 30 : map g [45,60,75]) !! 2) -- (Map.2)
                        in (map g [45,60,75] !! (2-1)) -- (!!.2)
                        in (((g 45): map g [60,75]) !! 1) -- (-) and (Map.2)
                        in ((map g [60,75]) !! (1-1)) -- (!!.2)
                        in (((g 60): map g [75]) !! 0) -- (-) and (Map.2)
                        in (g 60) -- (!!.1)
                        in ((++ "px").show) 60 -- Sub g
                        in (++ "px") (show 60) -- Func Comp
                        in (++ "px") "60" -- Show
                        in "60px" -- (++)
}
After, we find expressions which can be reduced still
map (f) [xs]

Notes:

!! - Double Bang - element look up
-}