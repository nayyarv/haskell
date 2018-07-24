module IsPrime where

sieveFilter :: [Int] -> Int -> [Int]
sieveFilter nums n 
    | i == 1 = []
    | n == length nums = nums
    | otherwise = sieveFilter (take n nums ++ [x | x <- drop n nums, x `mod` i /= 0]) (n+1)
    where i = head (drop (n-1) nums)


sieve :: Int -> [Int]
sieve n = sieveFilter [2..n] 1

isPrimeStepper :: Int -> Int -> Int -> Bool
isPrimeStepper x curr end
    | curr > end = True
    | x `mod` curr == 0 = False 
    | otherwise = isPrimeStepper x (curr+2) end

    
isPrimeE :: Int -> [Int] -> Bool
isPrimeE x sieveL
    | minimum modulos == 0 = False
    | otherwise = isPrimeStepper x 101 (floor (sqrt (fromIntegral x)))
    where modulos = [x `mod` i | i <- sieveL]



isPrimeH :: Int -> Bool
-- some optimizations 
isPrimeH x
    | x == 2 = True
    | x `mod` 2 ==  0 = False
    | x == 3 = True
    | x `mod` 3 == 0 = False
    | x `elem` sieveL = True 
    | otherwise = isPrimeE  x sieveL
    where sieveL = sieve 100


isPrime :: Integer -> Bool
isPrime x 
    | x <= 1 = False
    | otherwise = isPrimeH (fromInteger x)
