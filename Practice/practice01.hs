-- get list element by index (zero-based) yields element at index
elemAt :: [a] -> Int -> a
elemAt xs i =
  if i == 0
    then head xs
    else elemAt (tail xs) (i - 1)

-- determine whether a text string (ignoring spaces and punctuation) is a palindrome
isPalindrome :: String -> Bool
isPalindrome x =
  x == reverse x

-- break a list into two (roughly) equal-length disjoint sub-lists
partitionInHalf :: [a] -> ([a], [a])
partitionInHalf xs = splitAt half xs
  where
    half = (length xs + 1) `div` 2

-- provides a text representation of a date (day, month, year)
daySuffix :: Int -> String -- get the suffix for the day
daySuffix d
  | d < 1 || d > 31 = "Invalid day"
  | d `elem` [11, 12, 13] = "th"
  | d `mod` 10 == 1 = "st"
  | d `mod` 10 == 2 = "nd"
  | d `mod` 10 == 3 = "rd"
  | otherwise = "th"

monthName :: Int -> String -- get the month name
monthName m = case m of
  1 -> "January"
  2 -> "February"
  3 -> "March"
  4 -> "April"
  5 -> "May"
  6 -> "June"
  7 -> "July"
  8 -> "August"
  9 -> "September"
  10 -> "October"
  11 -> "November"
  12 -> "December"
  _ -> error "Invalid month"

convertYear :: Int -> Int -- convert two-digit year to four-digit year
convertYear y
  | y < 100 = 2000 + y
  | otherwise = y

showDate :: Int -> Int -> Int -> String
showDate d m y = show d ++ daySuffix d ++ " " ++ monthName m ++ ", " ++ show (convertYear y)

-- extract a sub-list containing those elements between ith (inclusive) and kth (exclusive) elements of a given list
slice :: [a] -> Int -> Int -> [a]
slice xs i k = 