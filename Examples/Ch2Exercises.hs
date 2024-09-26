-- Ex A:
{- Is a half of two plus two equal to two or three?
> half_of 2 `plus` 2
> 3
Function application
    is left associative f x y = (f x) y
    is highest precedence
f :: a -> b -> c
    -> is right associative
-}

-- Ex B:
{-
Is the syntax valid or not?
If yes, does the expression make sense (valid data type)

double :: Int -> Int

[0,1)                       - syntax error, brackets must match
double -3                   - type error meaning (double -) 3 in which '-' is subtraction
double (-3)                 - Good, overriding '-' from subtraction to negative
double double 0             - type error meaning (double double) 0, cannot double a function
if 1==0 then 2==1           - syntax error, every if must have an else. I.e. ... ==1 else 5 AND type error since types of expressions don't match
"++" == "+" ++ "+"          -
[(+),(-)]
[[],[[]],[[[]]]]
concat ["tea","for",'2']
concat ["tea","for","2"]

expressions reduce to a value
statements have an effect

Proper Syntax:
    if 1==0
        then 2==1
        else 5
-}

-- Ex C:
{-
Write a function modernize :: String -> String which ensures that paper titles are capitalized

1. toUpper :: Char -> Char converts a letter to uppercase
2. unwords :: [Word] -> String makes a list of words into a String

-}

-- ** Hoogle.haskell.org, Haskell search engine**, == is equal to & /= is not equal to