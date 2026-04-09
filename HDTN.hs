
fc = head [x | x <- triangles , divisors x > 500]

    where
        
        triangles  = scanl1 (+) [1..]
        divisors x = length [y |  y <- [1..sqrt1 x], x `mod` y == 0] * 2
        sqrt1 x    = floor . sqrt $ fromIntegral x

