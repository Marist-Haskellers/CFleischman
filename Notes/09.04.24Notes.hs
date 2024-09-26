-- Class Notes 9/4/24
xs = [1, 5, 3, 9, 6]

{-
> head xs
> 1
> :t head
> head :: GHC.Stack.Types.HasCallStack => [a] -> a
> last xs
> 6
-}
secondElem :: [a] -> a
secondElem ys = head (tail ys)

{-
> secondElem [2,4..100]
> 4
-}

{-
To write multiple lines in ghci

> :{
\| elemAtIndex :: [a] -> Int -> a
\| elemAtIndex xs i =
\|   if i = 0
\|       then head xs
\|       else elemAtIndex (tail xs) (i-1)        - searching through tail of list for idex of prev list w/ no head so (i-1)
> :}
-}
