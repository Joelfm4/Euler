
-- Sieve of Eratosthenes
primes :: [Integer] -> [Integer]
primes [] = []
primes (p:xs) = p : primes [x | x <- xs, x `mod` p /= 0]


-- Rule 6K +- 1
isPrime :: Integer -> Bool
isPrime n | n <= 1 = False
          | n <= 3 = True
          | n `mod` 2 == 0 || n `mod` 3 == 0 = False
          | any (\i -> n `mod` i == 0 || n `mod` (i + 2) == 0) [5, 11..q n] = False
          | otherwise = True
        
                where

                    q = floor . sqrt . fromIntegral


largestPrimeFactor :: Integer -> Integer -> Integer
largestPrimeFactor n factor
                            | factor * factor > n = n
                            | n `mod` factor == 0 = largestPrimeFactor (n `div` factor) factor
                            | otherwise           = largestPrimeFactor n (factor + 1)


