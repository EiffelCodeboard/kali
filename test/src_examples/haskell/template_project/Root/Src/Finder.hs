module Root.Src.Finder where

maxFunction :: [Int] -> Int
maxFunction [] = error "max of empty list"
maxFunction [x] = x
maxFunction (x:xs)
        | x > maxTail = x
        | otherwise = maxTail
        where maxTail = maxFunction xs
