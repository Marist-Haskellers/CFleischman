main :: IO ()
main = interact (showFilip . doFilip . parseFilip)

{-
    break input string into words
    reverse each word
    interpret each word as an integer
    compare the two integers to get the larger one
    convert larget into to a string representation

    show (maximum (map read (map reverse (words s))))
    -- using func comp
    (show . maximum . map read . map reverse . words) s

-}
parseFilip = words

doFilip :: [String] -> Int
doFilip = maximum . map (read . reverse)

showFilip = show