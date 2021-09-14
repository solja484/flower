{-# OPTIONS_GHC -Wall #-}
module Andrusiv01 where

-- Задача 1 -----------------------------------------
power3 :: [Integer]
power3 = [x^(3::Integer)|x<-[1..]]

-- Задача 2 -----------------------------------------
toPower3 :: [Integer]
toPower3 = [3^x|x<-[(1::Integer)..]]

-- Задача 3 -----------------------------------------
sumPower3 :: Integer -> Integer
sumPower3 n
 | n==0 = 1
 | n<0 = -1
 | otherwise = sum[3^x|x<-[1..n]]

-- Задача 4 -----------------------------------------
sumPower :: Integer -> Integer -> Integer
sumPower m n
 | m==0 = 0
 | n==0 = 1
 | m>=0 && n>0 = sum[m^i|i<-[1..n]]
 | otherwise = -1
 
-- Задача 5 -----------------------------------------
lessMe :: [Int] -> [Int]
lessMe xs = if null xs then [] else [lessNum x xs|x<-xs]
lessNum :: Int -> [Int] -> Int
lessNum x xs
 | null xs = 0 
 | x>(head xs) = 1+lessNum x (tail xs)
 | otherwise = lessNum x (tail xs)
 
-- Задача 6 -----------------------------------------
frequency :: [Int] -> [(Int,Int)]


removeRepeats :: [Int] ->[Int]
removeRepeats xs 
 | null xs = []
 | otherwise = head xs : removeRepeats (filter ((head xs) /=) xs)


frequency xs
 | null xs = []
 | otherwise = [(y, numbOfEnters y xs)|y<- removeRepeats xs]

numbOfEnters ::  Int -> [Int]-> Int
numbOfEnters x xs 
 | null xs = 0 
 | x == (head xs) = 1+numbOfEnters x (tail xs)
 | otherwise = 0+numbOfEnters x (tail xs)
 
-- Задача 7 -----------------------------------------
hailstone :: Int -> Int
hailstone x
 | x <= 1 = -1
 | even x = div x 2
 | otherwise = x * 3 + 1

-- Задача 8 -----------------------------------------
hailSeq :: Int -> [Int]
hailSeq x
 | x<=0 = []
 | otherwise = x:hailSeq(hailstone x)

-- Задача 9 -----------------------------------------
allHailSeq :: [[Int]]
allHailSeq = [hailSeq x|x<-[1..]]

-- Задача 10 -----------------------------------------
firstHailSeq :: Int -> Int
firstHailSeq l = calculateFirstHailSeq l allHailSeq
calculateFirstHailSeq :: Int->[[Int]]->Int
calculateFirstHailSeq l xs
 | l<=0 = -1
 | l==1 = 1
 | l == length(head xs) = head (head xs)
 | otherwise = calculateFirstHailSeq l (tail xs)