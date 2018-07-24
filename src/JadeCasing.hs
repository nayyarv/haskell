module JadenCasing where
import qualified Data.Char as Char

capitalize :: String -> String
capitalize str
    | length str == 0 = []
    | otherwise = [Char.toUpper (head str)] ++ (drop 1 str)

toJadenCaseInner :: String -> String -> String
toJadenCaseInner str output
    | length str == 0 = output
    | curr == ' ' = toJadenCaseInner (capitalize currTail) (output ++ [curr])
    | otherwise = toJadenCaseInner currTail (output ++ [curr])
    where curr = head str
          currTail = drop 1 str


toJadenCase :: String -> String
toJadenCase str
    | length str == 0 = str
    | otherwise = toJadenCaseInner (capitalize str) ""