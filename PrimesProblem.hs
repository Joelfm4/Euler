
isPrime :: Integer -> Bool
isPrime n | n <= 1 = False
          | n <= 3 = True
          | n `mod` 2 == 0 || n `mod` 3 == 0 = False
          | any (\i -> n `mod` i == 0 || n `mod` (i + 2) == 0) [5, 11..q n] = False
          | otherwise = True
        
                where

                    q = floor . sqrt . fromIntegral


-- ONLY potential prime numbers -_-
primes :: [Integer]
primes = 2 : 3 : filter isPrime candidates
        
            where
               candidates = concatMap (\k -> [6*k-1, 6*k+1]) [1..]



getPrime :: Int -> Integer 
getPrime n = primes !! (n-1)


