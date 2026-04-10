import Data.Ord (comparing)
import Data.List (maximumBy)


f :: Integer -> Integer
f n | even(n)   = n `div` 2
    | otherwise = 3*n + 1


seqLength :: Integer -> Int
seqLength n
    | n == 1 = 1
    | otherwise = 1+ seqLength (f n)


r = maximumBy (comparing seqLength) [750000..999999] -- OP: DP

