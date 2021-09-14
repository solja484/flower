{-# OPTIONS_GHC -Wall #-}
module HWP02 where

{-
foo :: [Int]-> [Int]
foo xs = map (cntLess xs) xs

cntLess :: [Int]->Int->Int
cntLess x xs= if null xs then 0 else (if head xs < x then 1 else 0) +cntLess (tail xs) xs
--cntLess x xs=length(filter(x>)xs)
-- уилл программирование на хаскеле
-}

--prefix - first 3 elements /4 5 etc
--allPrefix[1,2,3,4,5]=[[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]

{-allPrefix :: [Int]->[[Int]]
allPrefix xs
 | null xs = []
 | otherwise = [take i xs | i<-[1..length xs]]  

sumM :: [Int]->Int
sumM xs 
 | null xs = 0
 | otherwise = head xs + sumM (tail xs)

cumSumPrefix :: [Int]->[Int]
cumSumPrefix xs=map sumM(allPrefix xs) --((map sumM).allPrefix) xs
-}

notInList :: [Int]->Int->Bool
notInList xs x = null [x|x<-ys,y==x] -- =notElem x xs

diff :: [Int]->[Int]->[Int] -- різниця двох списків
-- diff 
diff xs ys = filter (notInList ys ) xs

minFree :: [Int]->Int
minFree xs = head (diff [0..] xs)






