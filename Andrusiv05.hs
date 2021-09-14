{-# OPTIONS_GHC -Wall #-}
module Andrusiv05 where

import Data.Char(isUpper)
import Data.List

type Grammar    = [Production]         -- КВ-граматика
type Production = (Char,String)        -- Правило виводу
type Predict    = [(Char,String)]      -- Прогнозуюча таблиця
type Control    = [((Char,Char),Int)]  -- Управляюча таблиця 

-- Задача 1 ------------------------------------
addOne :: String -> Char -> String  
addOne st c | contains st c = st 
 | otherwise = st++[c]

contains :: String-> Char ->Bool
contains st c | null st = False
 | (head st)==c = True
 | otherwise = contains (tail st) c

addAll :: String -> String -> String 
addAll st wd | null wd =  st
 | null st =  wd
 | not(contains st (head wd)) = addAll (st++[(head wd)]) (tail wd)
 | otherwise = addAll st (tail wd)

addWithout :: String -> String -> String 
addWithout st wd | null wd = sortBy sortGT1 st
 | null st = sortBy sortGT1 wd
 | (head wd)=='$' = addWithout st (tail wd)
 | not(contains st (head wd)) = addWithout (st++[(head wd)]) (tail wd)
 | otherwise = addWithout st (tail wd)

inter :: String -> String -> String 
inter st1 st2 | not (containAny st1 st2) = ""
 | not (contains st1 (head st2)) = inter st1 (tail st2)
 | not (contains st2 (head st1)) = inter (tail st1) st2
 | not (contains st2 (last st1)) = inter (init st1) st2
 | not (contains st1 (last st2)) = inter st1 (init st2)
 | st1==st2 = st1
 | length st1 > length st2 = myintersect st2 st1
 | otherwise = myintersect st1 st2
 
myintersect :: (Eq a) => [a] -> [a] -> [a]
myintersect xs ys =  [x | x <- xs, any ((==) x) ys] 

containAny :: String->String->Bool
containAny st1 st2 |null st1 = False
 | null st2 = False
 | contains st1 (head st2) = True
 | otherwise = containAny st1 (tail st2)

sortGT1 :: Ord a => a -> a -> Ordering
sortGT1 a1 a2
  | a1 < a2 = LT
  | a1 >= a2 = GT
-- Задача 2 ------------------------------------
--type Predict    = [(Char,String)]
tkPredict :: Predict -> Char -> String 
tkPredict pt n | null pt = ""
 | (fst(head pt))== n = snd(head pt)
 | otherwise = tkPredict (tail pt) n

upPredict :: Predict -> Char -> String -> Predict 
upPredict pt n st | tkPredict pt n =="" = sortBy sortGT ((n,st):pt)
 | otherwise = (map (changePr n st) pt)

sortGT :: Ord a => (a, b) -> (a, b) -> Ordering
sortGT (a1,_) (a2,_)
  | a1 < a2 = LT
  | a1 >= a2 = GT

changePr::  Char -> String ->(Char,String)->(Char,String)
changePr n st pt0 | fst pt0 == n = (n,st)
 | otherwise = pt0


-- Задача 3 ------------------------------------
parse ::  Grammar -> Control -> String -> Maybe [Int]
parse gr ctl input = doSteps gr ctl (input,[fst(head gr)]++"",Just [])

doSteps ::  Grammar -> Control -> (String, String, Maybe [Int]) -> Maybe [Int]
doSteps gr ctl (input,stack,result) 
 | input=="" && stack=="" = result
 | length stack == 1 && input=="" = doSteps gr ctl (step gr ctl ("$",stack,result))
 | input=="" && not(stack=="") = Nothing 
 | otherwise = doSteps gr ctl (step gr ctl (input,stack,result))



step :: Grammar -> Control -> 
       (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
step gr ctl (input,stack, result)
 | (stack=="" && input=="$")||(stack=="$"&&input=="$")||(stack=="$"&&input=="") = ("", "",result)
 | length stack == 1 && input=="" = step gr ctl ("$",stack,result)
 | not(isUpper (input!!0)) && not(isUpper (stack!!0)) && not(input!!0==stack!!0) = step gr ctl (tail input, tail stack,result)
 | not(isUpper (input!!0)) && not(isUpper (stack!!0)) && (input!!0==stack!!0) = (tail input,tail stack,result)
 | not(containsTerm ctl (stack!!0) (input!!0)) = (input,stack,Nothing)
 | otherwise = doCommand gr ctl (input,stack,result)

doCommand :: Grammar -> Control -> (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
doCommand gr ctl (input,stack, result) 
 | null ctl = (input,stack,result)
 | (fst(fst (head ctl))==stack!!0) && (snd(fst (head ctl))==input!!0) = (input, (newStack stack (gr!!(snd (head ctl)))),addCommand result (snd(head ctl)))
 | otherwise = doCommand gr (tail ctl) (input,stack, result) 

newStack::String->Production->String
newStack stack (_,newchar)= newchar++(tail stack) 

addCommand:: Maybe [Int]-> Int->Maybe [Int]
addCommand (Just result) num = Just (result++[num])
addCommand Nothing _=Nothing

containsNonTerm :: Control ->Char-> Bool
containsNonTerm ctl s
 | null ctl = False
 | fst(fst (head ctl))==s = True
 | otherwise = containsNonTerm (tail ctl) s
 
containsTerm ::Control->Char->Char->Bool
containsTerm ctl s a 
 | null ctl = False
 | (fst(fst (head ctl))==s) && (snd(fst (head ctl))==a) = True
 | otherwise = containsTerm (tail ctl) s a

-- Задача 4 ------------------------------------
first :: Predict -> String -> String
first pfst st | (st!!0)=='$' = first pfst (removeSign "" st '$')
 |  not(isUpper (st!!0)) = [st!!0]
 | otherwise = sortBy sortGT1 (removeUpperCase (firstStep pfst (doFirstStep pfst (removeRepeats st ""))))


firstStep :: Predict -> String -> String
firstStep pf st | (st!!0=='$') && (haveUpperCase st) = firstStep pf (doFirstStep pf (removeSign "" st '$'))
 | otherwise = sortBy sortGT1 st
 
doFirstStep:: Predict ->String->String
doFirstStep pf st | (fst(head pf))==(findUpperCase st) = removeSign "" (addAll (snd(head pf)) (st)) (findUpperCase st)
 | otherwise = doFirstStep (tail pf) st
 
removeSign:: String->String->Char->String
removeSign beg end s | null end = beg
 | end!!0==s = removeSign beg (tail end) s
 | otherwise = removeSign (beg++[head end]) (tail end) s

haveUpperCase:: String->Bool
haveUpperCase st | null st=False 
 | isUpper (head st) = True
 | otherwise = haveUpperCase (tail st)

findUpperCase:: String->Char
findUpperCase st | null st=' ' 
 | isUpper (head st) = head st
 | otherwise = findUpperCase (tail st)




removeRepeats:: String -> String -> String
removeRepeats s st 
 | s=="" = st
 | contains st (head s) = removeRepeats (tail s) st
 | otherwise = removeRepeats (tail s) (st++[head s]) 

removeUpperCase :: String->String
removeUpperCase s 
 | haveUpperCase s = removeUpperCase (removeSign "" s (findUpperCase s))
 | otherwise = s

-- Задача 5 ------------------------------------
buildingControl :: Grammar -> Predict -> Predict -> Control 
buildingControl = undefined

-- Задача 6 ------------------------------------
testingLL1 :: Grammar -> Predict -> Predict -> Bool
testingLL1 = undefined

fromGrammar :: Grammar ->  [(Char,[String])]
fromGrammar = undefined

testFst :: [String] -> Bool
testFst = undefined

testFollow :: String -> [String] -> Bool
testFollow = undefined

-- Задача 7 ------------------------------------
buildFst :: Grammar -> Predict 
buildFst gr = filterGr (buildFst1 gr (build gr [])) []

filterGr :: Predict->Predict->Predict
filterGr pr res | null pr = res
 | containsPr res (fst(head pr)) = filterGr (tail pr) res
 | otherwise = filterGr (tail pr) (res++[(head pr)])

buildFst1 :: Grammar -> Predict ->Predict 
buildFst1 gr pf | (evalFst gr pf) == pf = pf
 | otherwise = buildFst1 gr (evalFst gr pf)

build::Grammar->Predict->Predict
build gr pfst
 | null gr= pfst
 | snd(head gr) =="" && (containsPr pfst (fst(head gr))) = build (tail gr) (upPredict pfst (fst (head gr)) "$")
 | otherwise = build (tail gr) (upPredict pfst (fst(head gr)) "")
--
evalFst :: Grammar -> Predict -> Predict 
evalFst gr pFst 
 | null gr = pFst
 | otherwise = evalFst (tail gr) (extandFst pFst (head gr))


extandFst :: Predict -> Production -> Predict 
extandFst pFst (n,rul) = upPredict pFst n (sort $ addAll (tkPredict pFst n) (first pFst rul))

 
containsPr::Predict ->Char->Bool
containsPr pf ch | null pf = False
 | ch==fst(head pf) = True
 | otherwise = containsPr (tail pf) ch

-- Задача 8 ------------------------------------
buildNxt :: Grammar -> Predict -> Predict 
buildNxt = undefined

nontermTails :: Grammar -> [(Char,String)] 
nontermTails = undefined

evalNxt :: [(Char,String)] -> Predict -> Predict -> Predict
evalNxt = undefined

extandNxtOne :: Predict -> Char -> Predict -> String -> Predict
extandNxtOne = undefined

---------------------Тестові дані ---------------------------
 
gr0, gr1, gr2, gr3, gr4, gr5:: Grammar
--  LL(1)-граматики
gr0 = [('S',"aAS"),('S',"b"), ('A',"a"), ('A',"bSA")]  
gr1 = [('S',"TV"),('T',"d"),('T',"(S)"),('V',"+TV"),('V',"-TV"),('V',"")]  
gr2 = [('E',"TU"),('U',""),('U',"+TU"),('U',"-TU"),
       ('T',"FV"),('V',""),('V',"*FV"),('V',"%FV"),('V',"/FV"),
       ('F',"d"),('F',"(E)")] 
-- не LL(1)-граматики
gr3 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba"),('S',"")]
gr4 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]   
gr5 = [('E',"E+T"), ('E',"E-T"),('E',"T"), 
       ('T',"T*F"), ('T',"T%F"), ('T',"T/F"), ('T',"F"), 
       ('F',"d"),('F',"(E)") ]

-- прогнозуючі таблиці початкових терміналів Fst
pFst0, pFst1, pFst2, pFst3, pFst4, pFst5 :: Predict
pFst0 = [('A',"ab"),('S',"ab")]
pFst1 = [('S',"(d"),('T',"(d"),('V',"$+-")]
pFst2 = [('E',"(d"),('F',"(d"),('T',"(d"),('U',"$+-"),('V',"$%*/")]
pFst3 = [('A',"ab"),('S',"$a")]
pFst4 = [('E',"(d"),('F',"(d"),('T',"(d")]
pFst5 = [('E',"(d"),('F',"(d"),('T',"(d")]

-- прогнозуючі таблиці наступних терміналів Nxt
pNxt0, pNxt1, pNxt2, pNxt3, pNxt4, pNxt5 :: Predict
pNxt0 = [('A',"ab"),('S',"$ab")]
pNxt1 = [('S',"$)"),('T',"$)+-"),('V',"$)")]
pNxt2 = [('E',"$)"),('F',"$%)*+-/"),('T',"$)+-"),('U',"$)"),('V',"$)+-")]
pNxt3 = [('A',"$ab"),('S',"$b")]
pNxt4 = [('E',"$)+"),('F',"$)*+"),('T',"$)*+")]
pNxt5 = [('E',"$)+-"),('F',"$%)*+-/"),('T',"$%)*+-/")]   

-- управляючі таблиці 
ctl0, ctl1, ctl2 :: Control 
ctl0 = [(('A','a'),2),(('A','b'),3),(('S','a'),0),(('S','b'),1)]
ctl1 = [(('S','('),0),(('S','d'),0),(('T','('),2),(('T','d'),1),
        (('V','$'),5),(('V',')'),5),(('V','+'),3),(('V','-'),4)]
ctl2 = [(('E','('),0),(('E','d'),0),(('F','('),10),(('F','d'),9),
        (('T','('),4),(('T','d'),4),(('U','$'),1),(('U',')'),1),
        (('U','+'),2),(('U','-'),3),(('V','$'),5),(('V','%'),7),
        (('V',')'),5),(('V','*'),6),(('V','+'),5),(('V','-'),5),(('V','/'),8)]

