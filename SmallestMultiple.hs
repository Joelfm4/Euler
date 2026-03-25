
{- 
 -
 - Description: Evenly divisible by all numbers from 1 to 20
 -
 -}


-- First Solution
-- [x | x <- [21..], all (\y -> x `mod` y == 0)[1..20]]


-- Second Solution
-- [x | x <- [20, 40..], all (\y -> x `mod` y == 0)[1..20]]


-- Third Solution
-- [x | x <- [20, 40..], all (\y -> x `mod` y == 0)[11..20]]


-- Fourth Solution --_--
-- foldl1 lcm [11..20]
