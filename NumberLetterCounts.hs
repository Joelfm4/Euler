
count :: Int
count = sum (map aux [1..1000])

    where

        aux :: Int -> Int
        aux n 
           | n < 10    = r !! n
           | n < 20    = r1 !! (n - 10)
           | n < 100   = (r2 !! (n `div` 10)) + (r !! (n `mod` 10))
           | n < 1000  = r !! (n `div` 100) + hundred + if rem == 0 then 0 
                                                            else and + aux rem
           | n == 1000 = 11

            where

                hundred  = 7
                and      = 3
                rem      = n `mod` 100

        r   = [0, 3, 3, 5, 4, 4, 3, 5, 5, 4]
        r1  = [3, 6, 6, 8, 8, 7, 7, 9, 8, 8]
        r2  = [0, 0, 6, 6, 5, 5, 5, 7, 6, 6]

