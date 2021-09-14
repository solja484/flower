{-# OPTIONS_GHC -Wall #-}
module Andrusiv10 where


import Data.List
import qualified Text.ParserCombinators.Parsec as P



data RE = Null       | -- Нуль вираз
          Term Char  | -- Термінальний символ
          Seq RE RE  | -- Послідовність
          Alt RE RE  | -- Альтернатива
          Rep RE     | -- Повторення (*)
          Plus RE    | -- Повторення (+)
          Opt RE      -- Необов’язкове входження (?)
          deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

-- Задача 1 -----------------------------------------
simplify :: RE -> RE   
simplify (Plus r)= Seq (simplify r) (Rep (simplify r))
simplify (Opt r)= Alt (simplify r) Null
simplify (Seq r1 r2)=Seq (simplify r1) (simplify r2)
simplify (Alt r1 r2)=Alt (simplify r1) (simplify r2)
simplify (Rep r)=Rep (simplify r)
simplify Null = Null
simplify (Term t)=Term t

integer :: P.Parser Int
integer = do st <-P.many1 P.digit
             return (read st) 

-- Задача 2 -----------------------------------------
isTerminal :: Automation -> State -> Bool 
isTerminal (_, tst, _) st = elem st tst

isEssential :: Automation -> State -> Bool 
isEssential aut@(_,_,trans) st = (isTerminal aut st) || (isTrans trans st)

isTrans :: [Transition]->State->Bool
isTrans trans st | null trans = False
                 | ((getState(head trans)) == st) && (getLabel(head trans))/=Eps = True
                 | otherwise = isTrans (tail trans) st

getState :: Transition ->State
getState (st,_,_)=st

getLabel :: Transition ->Label
getLabel (_,_,lb)=lb
-- Задача 3 -----------------------------------------
transitionsFrom :: Automation -> State -> [Transition]
transitionsFrom (_,_,trans) st = filter (\x-> (getState x == st)) trans

-- Задача 4 -----------------------------------------
labels :: [Transition] -> [Label]
labels trans= nub $ filter (\x-> ( x /= Eps)) label
               where label = [getLabel t|t<-trans]

-- Задача 5 -----------------------------------------
acceptsDA :: Automation -> String -> Bool
acceptsDA aut@(st0,_,_) str = if isDetermined aut then acceptsDaStr aut str st0 else False

acceptsDaStr :: Automation -> String -> State-> Bool
acceptsDaStr aut@(_,_,trans) str st | null str = isTerminal aut st 
                                    | otherwise = let tr = [t|t<-trans,  (C (head str))==(getLabel t), (getState t)==st]
                                                    in if null tr then False
                                                       else (acceptsDaStr aut (tail str) (getNextStates(head tr)))

isDetermined ::Automation->Bool
isDetermined au@(_,_,trans) = (withoutEps trans)  && (null (filter (\r1->(length $ labels r1)/=(length [lb|(_,_,lb)<-r1])) r))
                                               where r= [transitionsFrom au(getState t) |t<-trans ] 


withoutEps::[Transition]->Bool
withoutEps ((_,_,label):xs) | label == Eps = False
                            | otherwise = withoutEps xs
withoutEps []=True
-- Задача 6 -----------------------------------------
stStep  :: Automation -> State -> Label -> [State]
stStep (_, _, trans) state lb = [getNextStates x | x <- trans, getLabel x == lb, getState x == state]


setStep :: Automation -> [State] -> Label -> [State]
setStep (_, _, trans) states lb = [getNextStates x | x <- trans, st <- states, getLabel x == lb, getState x == st]


closure :: Automation -> [State] -> [State]
closure aut@(_, _, trans) states | sort res == sort states = res
                                 | otherwise = helpClosure aut (nub (res)) 
                                   where res = nub ([getNextStates x | x <- trans, st <- states, getLabel x == Eps, getState x == st])


helpClosure:: Automation -> [State] -> [State]
helpClosure aut@(_, _, trans) states | sort res == sort states = res
                                     | otherwise = helpClosure aut (nub (res)) 
                                   where res = nub ([getNextStates x | x <- trans, st <- states, getLabel x == Eps, getState x == st] ++ states)
getNextStates :: Transition ->State
getNextStates (_,stt,_)=stt


-- Задача 7 -----------------------------------------
accepts :: Automation -> String -> Bool
accepts aut@(st0,_,_) str | isDetermined aut = acceptsDA aut str
                          | otherwise = acceptsNDA aut str st0

acceptsNDA :: Automation -> String -> State-> Bool
acceptsNDA aut@(_,_,trans) str st | null str = isTerminal aut st 
                                    | otherwise = let full = [t|t<-trans,  (C (head str))==(getLabel t), (getState t)==st]
                                                      empty = [t|t<-trans,  (Eps)==(getLabel t), (getState t)==st]
                                                  in if (null full)&&(null empty) then False
                                                     else   if (not (null ([acceptsNDA aut (tail str) (getNextStates f)|f<-full]) ))
                                                            then (acceptsNDA aut (tail str) (getNextStates (head full)))
                                                            else or[acceptsNDA aut str (getNextStates e)|e<-empty]
-- Задача 8 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int) 
make Null start end next = ([(start, end, Eps)],  next)

make (Term ch) start end next = ([(start, end, C ch)],  next)

make (Seq r1 r2) start end next = (fst recur1 ++ [(next, next+1, Eps)] ++ fst recur2, snd recur2)
                                  where recur1 = make r1 start next (next+2)
                                        recur2 = make r2 (next+1) end (snd recur1)

make (Rep r1) start end next = ([(start, end, Eps)] ++ [(start, next, Eps)] 
                                                    ++ fst recur ++ [((next + 1), next, Eps)] 
                                                                 ++ [(next + 1, end, Eps)], (snd recur))
                                where recur = make r1 next (next + 1) (next+2)
                                
make (Alt r1 r2) start end next = ([(start, next, Eps)] ++ fst recur1 ++ [(next+1, end, Eps)] 
                                                        ++ [(start, next+2, Eps)] ++ fst recur2 ++ [(next+3, end, Eps)], snd recur2)
                                where recur1 = make r1 next (next + 1) (next+4)
                                      recur2 = make r2 (next + 2) (next + 3) (snd recur1)

make (Plus _) _ _ _ = error "simplify error"
make (Opt _) _ _ _ = error "simplify error"


-- Задача 9 -----------------------------------------
parseReg :: String -> Maybe RE 
parseReg _ = Prelude.Nothing

-- Задача 10 -----------------------------------------
getFrontier :: State -> Automation -> [Transition]
getFrontier  = undefined

getFrontier' :: State -> Automation -> [Transition] -> [Transition]
getFrontier' = undefined
-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigureS, re1S, re2S, re3S, re4S, re5S, re6S :: String
reFigureS = "(a|b)*c"
re1S = "(x|y)(1|2)"
re2S = "x'*"
re3S = "(ab|c)*"
re4S = "(a?)a"
re5S = "(ab)?d+"
re6S = "c?*"

reFigure, re1, re2, re3, re4, re5, re6 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Opt(Term 'a')) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))
re6 = Rep (Opt (Term 'c'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, nda6, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5, da6 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

nda6 = (1,[2], [(1,2,Eps),(1,3,Eps),(3,5,Eps),(5,6, C 'c'), (6,4,Eps), 
                (4,2,Eps), (3,7,Eps), (7,8,Eps), (8,4,Eps), (4,3,Eps)]) 
da6 = (1,[1], [(1,1, C 'c')])

ndaTest = (1, [1], [(1,2, C 'a'), (1,4, Eps), (1,3, C 'b'), (2,3, Eps),
              (3,5, Eps), (3,4, C 'a'), (4,4, Eps), (4,1, Eps), (5,2, Eps), (5,4,Eps)] )