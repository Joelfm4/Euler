
sumSquareDifference :: Int -> Int
sumSquareDifference n = x - y

                        where 
                              x = ( sum [1..n] ) ^ 2
                              y = sum [x^2 | x <- [1..n]]


sumSquareDifference1 :: Int -> Int
sumSquareDifference1 n = x - y

                        where 
                              x = ( foldl (+) 0 [1..n] ) ^ 2 
                              y = foldl (+) 0 [x^2 | x <- [1..n]]


sumSquareDifference2 :: Int -> Int
sumSquareDifference2 n = x - y

                        where 
                              x = ( foldl (+) 0 [1..n] ) ^ 2 
                              y = foldl (\a i -> a + i^2) 0 [1..n]



