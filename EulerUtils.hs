module EulerUtils
(
    numberOfDigits,
    takeDigits,
    isPrime,
    primesUpTo,
    primes,
    aproxPrimeSumUpTo,
    sumUpTo
) where


-------------------------------------------------
--                 Numbers                     --
-------------------------------------------------

numberOfDigits :: Int -> Int
numberOfDigits n
    | n == 0    = 1
    | otherwise = logarithm + 1
        
        where
            
            logarithm = floor ( logBase 10 (fromIntegral (abs n) :: Double) )
            


takeDigits :: Int -> Int -> Int
takeDigits n d
    | d <= 0           = error "Digits must be positive"
    | d > totalDigits  = error "Requested digits exceed number length"
    | d == totalDigits = n
    | otherwise        = numerator `div` denominator
    
        where

            denominator = 10 ^ exponent
            exponent    = floor (logBase 10 (fromIntegral numerator)) + 1 - d
            numerator   = abs (n)
            totalDigits = numberOfDigits (abs(n))


-------------------------------------------------
--                 Primes                      --
-------------------------------------------------

-- Algorithm : Trial Division Primality Test
isPrime :: Integer -> Bool
isPrime n | n <= 1 = False
          | n <= 3 = True
          | n `mod` 2 == 0 || n `mod` 3 == 0 = False
          | any (\i -> n `mod` i == 0 || n `mod` (i + 2) == 0) [5, 11..q n] = False
          | otherwise = True
        
                where

                    q = floor . sqrt . fromIntegral


-- Algorithm : Sieve of Eratosthenes
primesUpTo :: Int -> [Int]
primesUpTo n = sieve [2..n]

    where
       
        sieve :: [Int] -> [Int]
        sieve [] = []
        sieve(x:xs)
            | x*x > n = x : xs
            | otherwise = x : sieve [i | i <- xs, i `mod` x /= 0]


-- Asymptotic expansion
aproxPrimeSumUpTo :: Int -> Integer
aproxPrimeSumUpTo n = p1 + p2

                    where
                        nd = fromIntegral n :: Double
                        ln = log nd

                        p1 = floor $ nd^2 / (2 * ln)
                        p2 = floor $ nd^2 / (4 * ln^2)


-- Note: Zero-based index
primes :: [Integer]
primes = 2 : 3 : filter isPrime candidates
        
            where

               candidates = concatMap (\k -> [6*k-1, 6*k+1]) [1..]



sumUpTo :: Int -> Int 
sumUpTo n = sum $ primesUpTo n


-------------------------------------------------
--                   Other                     --
-------------------------------------------------
