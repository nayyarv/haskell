module FoldedList where

fold :: [Int] -> [Int] -> [Int]
fold inList outList
    | inList == [] = outList
    | n == 1 = outList ++ [frst]
    | otherwise = fold middle (outList ++ [(frst+lastIt)])
    where n = length inList
          frst = head inList 
          middle = tail (init inList)
          lastIt = last inList


foldList :: [Int] -> Int -> [Int]
foldList list numTimes
    | numTimes == 1 = oneFold
    | otherwise = foldList oneFold (numTimes-1)
    where oneFold = fold list []