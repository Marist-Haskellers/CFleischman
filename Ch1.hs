{-
    Count the occurrences of the n most common words in a text
-}

import Data.Char

type Text = String -- type synonym

countRuns = undefined

showRuns = undefined

sortRuns = undefined

sortWords = undefined

commonWords :: Int -> Text -> String
commonWords n = concat . map showRuns . take n . sortRuns . countRuns . sortWords . words . map toLower
