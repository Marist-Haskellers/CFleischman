{-
Haskell Lists:
    an immutable singly-linked list
    [1,->] [2,->] [3,->] [4,X]

Recursive, higher order type:
a - type variable
: / cons - construct operator
data List a = Nil | Cons a (List a)

List functions:
    head, tail      - constant time O(1) "big-oh of n"
    last, init      - O(n) linear time
    null            - O(1)
    (:)             - O(1)
    (++)            - O(n)
    concat
    takeWhile, take
    nub - in Data.List
    elemIndex - in Data.List
    zipWith
    zip
\*   sum
    map, filter
    split, splitAt
    drop, dropWhile

Enumeration (Enum):
[1..5]       - inclusive bound at 5
[1, 3.. 15]
[1..]

> ghci> :t [1..]
>       [1..] :: (Num a, Enum a) => [a]

[5,4..1]     - can count down if given 2nd element
['A'..'Z']

List comprehension:
[n^2 | n <- [1..]] - list of all ^2 of positive whole numbers
[1,4,9,16...] - cannot be written in Haskell

Syntactic Sugar - a feature of the languages syntax that makes it easier/quicker to do something
[1,2,3,4] => 1:2:3:4:[]

Map function:
> ghci> map (*10) [1..10]
>       [10,20,30,40,50,60,70,80,90,100]

> ghci> xs = [1..10]
> ghci> map (*10) xs
>       [10,20,30,40,50,60,70,80,90,100]
> ghci> xs
>       [1,2,3,4,5,6,7,8,9,10] - list are immutable, it'll just make a new list

> ghci> words = ["Apple", "Banana", "Cherry", "Date", "Fig"]
> ghci> map length words
>       [5,6,6,4,3]

:m - in ghci use this to import a moduel
> ghci> :m Data.Char
> ghci> :t map toUpper
>       map toUpper :: [Char] -> [Char]
> ghci> :t words
>       words :: [String]

> ghci> allToUpper (x:xs) = map toUpper x : allToUpper xs; allToUpper [] = []
CAN BE WRITTEN AS
> ghci> map(map toUpper) words

; - used to combine two lines in ghci

Filter function:
> ghci> :t filter
>       filter :: (a -> Bool) -> [a] -> [a]

> ghci> filter (== "apple") words
>       ["Apple"]

> ghci> (=='a') (head "apple")
>       True

Get all first letters, filter for 'a', get the length
> ghci> length (filter (=='a') (map head words))

> ghci> filter ((=='a') . head) words
> ghci> startsWithLowerA word = head word == 'a'
> ghci> startsWithLowerA "apple"
>       True

> ghci> startsWithLowerA word = (==) (head word) 'a'
> ghci> startsWithLowerA word = (==) 'a' (head word)
> ghci> startsWithLowerA word = ((==) 'a' . head) word
> ghci> startsWithLowerA = ((==) 'a' . head)

Lambda function notation:
\ - makes a function without a name
-> - points from parameter list to the implementation/result
> ghci> filter (\word -> head word == 'a') words

> ghci> ys = map (\x -> x^2 + 2*x - 1) [0..]
> ghci> ys !! 5
>       34

Functor - :i Functor more general mapping
> ghci> f = map length
> ghci> f words
>       [5,6,6,4,3]

> ghci> :t length
>       length :: Foldable t => t a -> Int

> ghci> :t map length
>       map length :: Foldable t => [t a] -> [Int]

> ghci> x = Nothing
> ghci> y = Just 6

> ghci> fmap(*2) x
>       Nothing
> ghci> fmap(*2) y
>       Just 12
-}
