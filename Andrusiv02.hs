{-# OPTIONS_GHC -Wall #-}
module Andrusiv02 where

-- Завдання 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl xs | null xs = 0 
 | otherwise = foldl (+) 0 xs
  
-- Завдання 2 ----------------------------------------- 
productFr :: [Integer] -> Integer
productFr xs | null xs = 0 
 | otherwise = foldr (*) 1 xs

-- Завдання 3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys | (null xs)&&(null ys) = []
 | null xs = ys
 | null ys = xs 
 | otherwise =  flip (foldr (:)) xs ys
 
-- Завдання 4 -----------------------------------------
sortInsert :: [Int] -> [Int]
sortInsert xs  = foldl insert [] xs

insert :: [Int] -> Int -> [Int]
insert xs v 
 | null xs = [v]
 | otherwise = filter (<v) xs ++ [v] ++ filter (>=v) xs 

-- Завдання 5 -----------------------------------------
findIndices ::(Int -> Bool) -> [Int] -> [Int] 
findIndices p xs | null xs = []
 | otherwise = [i | (i, x) <- zip [0..] xs, p x]

-- Завдання 6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse xss | null xss = []
 | otherwise = reverse(last xss):allReverse (init xss)

-- Завдання 7  -----------------------------------------

noDigits :: String -> String
noDigits xs = filter (not . isDigit) xs

isDigit :: Char -> Bool
isDigit n = elem n ['0'..'9']


-- Завдання 8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood ps v
 | null ps = 0
 | head ps v = 1 + cntGood (tail ps) v
 | otherwise = 0 + cntGood (tail ps) v

-- Завдання 9 ------------------------------------------
trianglePas :: [[Integer]]
trianglePas = iterate nextRow [1]

nextRow :: [Integer] -> [Integer]
nextRow xs = [1] ++ zipWith (+) xs (tail xs) ++ [1]

-- Завдання 10 -----------------------------------------
factorialM :: [Integer]
factorialM =  1 : zipWith (*) factorialM [2..]