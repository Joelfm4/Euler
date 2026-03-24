
isPalindrome :: Int -> Bool
isPalindrome n  | n < 0  = False 
                | otherwise = s == reverse s

                    where s = show n
                
{- Pseudocode
 -
 - long largest = 0;
 -
 - for(int i = 999; i > 0; i--)
 -
 -  long current = 0;
 -
 -  for(int j = 999; j > 0; j--)
 -
 -      current = i*j;
 -
 -      if(isPalindrome(current))
 -
 -          if(crrent >= largest) largest = current;
 -
 -          else return largest;
 -
 - -}


-- Solution: maximum [x*y | x <- [999,998..0], y <- [999,998..0], isPalindrome(x*y)]

