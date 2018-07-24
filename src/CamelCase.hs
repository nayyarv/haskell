module CamelCase where


import qualified Data.Char as Char

capitalize :: String -> String
capitalize str 
    | str == [] = []
    | otherwise = [Char.toUpper (head str)]

drop2 :: String -> String
drop2 [] = []
drop2 str
    | length str == 1 = []
    | otherwise = tail (tail str)

toCChelp:: String -> String -> String
toCChelp curr  targ
    | curr == [] = targ
    | elem (head curr) "-_" = toCChelp (capitalize(tail curr) ++ (drop2 curr)) (targ )
    | otherwise = toCChelp (tail curr) (targ ++ [head curr])

toCamelCase :: String -> String
toCamelCase [] = []
toCamelCase str = toCChelp str ""

