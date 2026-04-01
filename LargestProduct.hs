import Data.Char (digitToInt)

largestProduct' :: String -> Int -> Int
largestProduct' s best 
    | length s < 13 = best
    | otherwise = largestProduct' (drop 1 s) (max best current) 

        where

            current = product . map digitToInt $ take 13 s


-- Too many zeros :/

