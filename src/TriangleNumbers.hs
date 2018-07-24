module TriangleNumbers where

sumSeq :: Integer -> Integer
sumSeq n
    | n <=0 = 0
    | n == 1 = 1
    | otherwise = quot (n * (n + 1))  2


sumTri :: Integer -> Integer -> Integer
sumTri n help
    | n <=0 = help
    | otherwise = sumTri (n-1) (sumSeq(n) + help)


sunTriNumbers :: Integer -> Integer
sunTriNumbers n = sumTri n 0