module TenMinuteWalk where

isValidWalk :: [Char] -> Bool
isValidWalk swalk
    | n /= 10 = False
    | numn /= nums = False 
    | nume /= numw = False
    | otherwise = True

    where walk = take 11 swalk
          n = length walk 
          numn = length (filter (=='n') walk)
          nums = length (filter (=='s') walk)
          nume = length (filter (=='e') walk)
          numw = length (filter (=='w') walk)