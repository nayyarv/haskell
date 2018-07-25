module RomanNumerals where


-- symb :: [(Int, Char)]
symbology = [(1000, 'M'), (500, 'D'), (100, 'C'), (50, 'L'), (10, 'X'), (5, 'V'), (1, 'I')]

solution :: Integer -> String
solution n = "hi"


solbuilder :: Integer -> Int -> String -> String
solbuilder currNum symindex outString
    | symindex == length symbology || currNum <= 0 = outString
    | numletts == 4 && currRomLet /= 'M' = solbuilder remaining (symindex+1) (outString ++ four)
    | numletts > 0 && numletts < 4  = solbuilder remaining (symindex+1) (outString ++ take numletts (repeat currRomLet))
    | otherwise = solbuilder currNum (symindex+1) outString
    where currRomNum = fst (symbology!!symindex)
          currRomLet = snd (symbology!!symindex)
          numletts = fromInteger (currNum `div` currRomNum)
          remaining = currNum `mod` currRomNum
          four = [currRomLet, snd (symbology!! (symindex-1))]


