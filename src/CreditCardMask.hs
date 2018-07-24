module Maskify where


maskify :: String -> String
maskify str
    | n <= 4 = str
    | otherwise = replicate (n-4) '#' ++ drop (n -4) str
    where n = length str
