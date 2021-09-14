{-# OPTIONS_GHC -Wall #-}
module Andrusiv03 where
type Algorithm    = [Substitution]
type Substitution = (String,String,Bool)
type ConfigA      = (Bool, Int, String)

data Command = Z Int | S Int | T Int Int | J Int Int Int deriving Show
type Program = [Command]
type ConfigC = (Int, Int, [Int])

-- Задача 1 ------------------------------------
isPrefix :: String -> String -> Bool 
isPrefix bs xs 
 | bs=="" = True
 | otherwise = fst(splitAt (length bs) xs) == bs 

-- Задача 2 ------------------------------------
substitute :: Substitution -> Int -> String -> String
substitute sub i w
 | i>(length w) = w
 | check sub w i = fst (splitAt i w) ++ (exchange sub (snd(splitAt i w)) )
 | otherwise = w

returnFirst :: Substitution->String
returnFirst (a,_,_)=a

returnSecond :: Substitution->String
returnSecond (_,b,_)=b

returnThird :: Substitution->Bool
returnThird (_,_,c)=c

-- return if the part of word from i index is similar to that we need
check:: Substitution->String->Int->Bool
check sub w i = fst (splitAt (length (returnFirst sub)) (snd(splitAt i w))) == returnFirst sub

--change first left entry of element
exchange :: Substitution -> String ->String
exchange sub str = returnSecond(sub) ++ snd (splitAt(length (returnFirst sub)) str)


-- Задача 3------------------------------------
findPositionInt :: String -> Substitution -> [Int] 
findPositionInt w sub = [ i | (x,i)<-zip (findPos w sub) [0,1..], x] 

findPosition :: String -> Substitution -> [(Substitution, Int)] 
findPosition w sub = [(sub, i)|i<-(findPositionInt w sub)]

findPos :: String -> Substitution -> [Bool] 
findPos  w sub= [check sub w i|i<-[0..(length w)]]


-- Задача 4 ------------------------------------
findAll :: Algorithm -> String -> [(Substitution,Int)]  
findAll algo w
 | null algo = []
 | otherwise = findPosition w (head algo)++findAll (tail algo) w

-- Задача 5 ------------------------------------
stepA :: Algorithm -> ConfigA -> ConfigA
stepA algo (bt,st,word)
 | bt==False = (bt,st,word)
 | null (findAll algo word) = (False,st,word)
 | otherwise = (not (returnThird (fst(head (findAll algo word)))),st+1,substitute (fst(head (findAll algo word))) (snd(head (findAll algo word))) word)


-- Задача 6 ------------------------------------

evalA :: Algorithm -> Int -> String -> Maybe String 
evalA algo m word
 | m<=0 = Nothing
 | ((endAlgorithm algo (True,0,word) m)==False) = Nothing
 | otherwise = Just (doAlgorithm algo (True,0,word))

doAlgorithm :: Algorithm -> ConfigA -> String
doAlgorithm algo conf0 
 | ((returnFirstConf conf0)==False) = returnThirdConf conf0
 | otherwise = doAlgorithm algo (stepA algo conf0)

endAlgorithm :: Algorithm -> ConfigA -> Int -> Bool
endAlgorithm algo conf0 m 
 | (((returnFirstConf conf0)==False)&&((returnSecondConf conf0)==(m-1))) = True
 | (((returnFirstConf conf0)==False)&&(not((returnSecondConf conf0)==(m-1)))) = False
 | otherwise = endAlgorithm algo (stepA algo conf0) m

returnFirstConf :: ConfigA->Bool
returnFirstConf (a,_,_)=a

returnSecondConf :: ConfigA->Int
returnSecondConf (_,b,_)=b

returnThirdConf :: ConfigA->String
returnThirdConf (_,_,c)=c


{-
data Command = Z Int | S Int | T Int Int | J Int Int Int deriving Show
type Program = [Command]
-}
-- Задача 7 ------------------------------------
maximReg :: Program -> Int 
maximReg pr= calcMax 0 pr

calcMax :: Int->Program->Int
calcMax x pr
 | null pr = x
 | otherwise = calcMax (max x (getMaxElem (head pr))) (tail pr)
 
getMaxElem :: Command -> Int
getMaxElem (Z x) = x
getMaxElem (S x) = x
getMaxElem (T x y) = max x y
getMaxElem (J x y _) = max x y

-- Задача 8 ------------------------------------
ini :: Program -> [Int] -> [Int] 
ini pr ir=ir++[0|_<-[0..((maximReg pr)-(length ir)-1)]] 

upd :: [Int] -> Int -> Int-> [Int]
upd reg r v 
 | r<0 = reg
 | otherwise = (fst(splitAt r reg))++[v]++(snd(splitAt(r+1) reg))


{-
type Program = [Command] [Z 1, S 2, J 1 1 4]
type ConfigC = (Int, Int, [Int])
-}
-- Задача 9 ------------------------------------
stepC :: Program -> ConfigC -> ConfigC
-- 
-- notSignum = [Z 2, J 1 2 5, Z 1, J 1 1 6, S 1] 
stepC pr (nm,st,rg) 
 | nm >(length pr) ||( nm<0 )= (-1,st,rg) 
 | otherwise = (setNextCommand (pr!!(nm-1)) nm rg (length pr),st+1,doFunc(pr!!(nm-1)) rg)

setNextCommand:: Command->Int->[Int]->Int->Int
setNextCommand (Z _) nm _ prl
 | nm==prl = -1
 | otherwise = nm+1
setNextCommand (S _) nm _ prl
 | nm== prl= -1
 | otherwise = nm+1
setNextCommand (T _ _) nm _ prl
 | nm==prl= -1
 | otherwise = nm+1
setNextCommand (J x y n) nm reg prl
 | ((reg!!(x-1))==(reg!!(y-1)) )&&n>0 &&(n<=prl)= n
 | ((reg!!(x-1))==(reg!!(y-1)) )&&n>0 &&(n>prl)= -1
 | (not((reg!!(x-1))==(reg!!(y-1)) ))&&(nm==prl)= -1
 | otherwise = nm+1

doFunc :: Command ->[Int]->[Int]
doFunc (Z x) reg=upd reg (x-1) 0 
doFunc (S x) reg=upd reg (x-1) (reg!!(x-1)+1)
doFunc (T x y) reg= upd reg (y-1) (reg!!(x-1))
doFunc (J _ _ _) reg = reg 


-- Задача 10 ------------------------------------
evalC :: Program -> Int -> [Int] -> Maybe Int 
evalC pr mx ir
 | mx<=0 = Nothing
 | not(endAlg pr (1,0,ir) mx) = Nothing
 | otherwise = Just (doAlg pr (1,0,ir))


endAlg:: Program -> ConfigC ->Int-> Bool
endAlg pr conf0 mx 
 | (returnFirstConfC conf0 == -1 ) && (returnSecondConfC conf0 ==mx) = True
 | (not(returnFirstConfC conf0 == -1 )) && (returnSecondConfC conf0 ==mx) = False
 | otherwise = endAlg pr (stepC pr conf0) mx 
 
doAlg:: Program ->ConfigC->Int
doAlg pr conf0
 | returnFirstConfC conf0 == -1 = (returnThirdConfC conf0)!!0
 | otherwise = doAlg pr (stepC pr conf0)

returnFirstConfC :: ConfigC->Int
returnFirstConfC (a,_,_)=a

returnSecondConfC :: ConfigC->Int
returnSecondConfC (_,b,_)=b

returnThirdConfC :: ConfigC->[Int]
returnThirdConfC (_,_,c)=c
---------------------Тестові дані - Нормальні алгоритми Маркова ---------------------------
clearBeginOne, addEnd, reverse, multiply:: Algorithm 
-- стирає перший символ вхідного слова (алфавіт {a,b})
clearBeginOne = [ ("ca", "", True)
                , ("cb", "", True)
                , ("", "c", False)
                ] 

-- дописує abb в кінець вхідного слова (алфавіт {a,b})
addEnd = [ ("ca", "ac", False)
         , ("cb", "bc", False)
         , ("c", "abb", True)
         , ("", "c", False)
         ] 
-- зеркальне відображення вхідного слова (алфавіт {a,b})
reverse = [ ("cc", "d", False)
          , ("dc", "d", False)
          , ("da", "ad", False) 
          , ("db", "bd", False) 
          , ("d", "", True) 
          , ("caa", "aca", False) 
          , ("cab", "bca", False) 
          , ("cba", "acb", False)
          , ("cbb", "bcb", False) 
          , ("", "c", False) 
          ]

-- добуток натуральних чисел 
--  multiply ("|||#||") = "||||||"  3*2 = 6
multiply = [("a|", "|ba", False)
            ,("a", "", False)
            ,("b|", "|b", False)
            ,("|#", "#a", False)
            ,("#", "c", False)
            ,("c|", "c", False)
            ,("cb", "|c", False)
            ,("c", "", True)
            ]

---------------------Тестові дані - Програми МНР ---------------------------
notSignum, addition, subtraction :: Program 
-- функція notSignum x
notSignum = [Z 2, J 1 2 5, Z 1, J 1 1 6, S 1] 

-- функція додавання  addition x y = x+y
addition = [Z 3, J 3 2 6, S 1, S 3, J 1 1 2]

-- функція віднімання subtraction x y = x-y, визначена для x>=y 
subtraction = [Z 3, J 1 2 6, S 2, S 3, J 1 1 2, T 3 1]