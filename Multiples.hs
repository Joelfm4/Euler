

sumMultiples:: Int -> Int
sumMultiples 0 = 0
sumMultiples n = sum [x | x <- [1..n-1], x `mod` 3 == 0 || x `mod` 5 == 0]


