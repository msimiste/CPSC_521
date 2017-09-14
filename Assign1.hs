
myApp:: ([a],[a]) -> [a]
myApp ([], ys)= ys
myApp ((x:xs),ys) = x:(myApp(xs,ys))

myRev:: [a] -> [a]
myRev xs = rev xs []
    where
        rev [] a = a
        rev (x:xs) ys = rev xs (x:ys)
        
-- myFlatten:: [[a]] -> [a]
-- myFlatten []  = []
-- myFlatten [xs] = flat 
    -- where
        -- flat lst:_
            -- | lenList == 1 = lst
            -- | otherwise = myFlatten lst:_
            -- where lenList = length lst:_
            
factorial:: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

greaterInList:: Integer -> [Integer] -> [Integer]
greaterInList _ [] = []
greaterInList x ys = foldr (\b acc  -> if b > x  then b:acc else acc) [] ys

msplit:: [a] -> ([a],[a])
msplit xs = foldr (\x  (e,o) n -> if n mod 2 == 0 then (x:e,o) else (e,x:o) ) ([],[]) xs 0

