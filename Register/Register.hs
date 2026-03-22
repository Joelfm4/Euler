

squareNumbers :: Int -> [Int]
squareNumbers 0 = []
squareNumbers n = [x^2 | x <- [1..n]]


sumSquareNumbers :: Int -> Int
sumSquareNumbers 0 = 0
sumSquareNumbers n = sum [x^2 | x <- [1..n]]


sumOddSquareNumbers :: Int -> Int
sumOddSquareNumbers 0 = 0
sumOddSquareNumbers n = sum [x^2 | x <- [1..n], odd (x^2)]

