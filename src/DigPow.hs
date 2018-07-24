module DigPow where

sumPow :: Integer -> Integer -> Integer -> Integer
sumPow number pow curr
    | number >= 10 = sumPow quotient (pow -1) (curr + remain ^ pow)
    | otherwise = curr + number ^ pow
    where quotient = number `div` 10
          remain = number `mod` 10


digpow :: Integer -> Integer -> Integer
digpow n p 
    | ss `mod` n == 0 = ss `div` n
    | otherwise = -1
    where ss = sumPow n (numdigits+p-1) 0
          numdigits = toInteger (length (show n))

