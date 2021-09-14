{-# OPTIONS_GHC -Wall #-}
module Andrusiv009 where 

import Data.List 
type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition (State,State,Label)
type Automation = (State,[State],[Transition])


states::Automation->[State]
states (ist, fsx, trx)= (sort . nub . concat) [[ist], fsx, concatMap (\(s1,s2,_)->[s1,s2]), trx]
isDeter:: Automation ->Bool
isDeter (_,_,trx)= (null.(filter (\(_,_,l)->l==Eps))) trx &&
                   (null .(filter ((>1).length)).group.sort.(map(\(s,_,l)->(s,l)))) trx
{-
data Branch=Leaf Int|Fork Branch Branch deriving (Eq, Show)

frigle::Branch ->[Int]
frigle (Leaf v)=[v]
frigle (Fork l r)=frigle l ++ frigle r

height:: Branch ->Int
height (Leaf _)=0
height (Fork l r) = maximum [height l,height r] +1

minOne::[Int]->Branch
minOne [v]= (Leaf v)
minOne xs = let d=div (length xs) 2
                l=minOne(take d xs)
                r=minOne (drop d xs)
            in  (Fork l r)

--minBr :: [Int]->[Branch]
brs::  [Int] ->[Branch]
brs [v]= [Leaf v]
brs xs = [Fork l r | i<-[1..length xs -1],
                     l<-brs (take i xs),
                     r<-brs (drop i xs)]
takeMinBr :: [Branch]->[Branch]
takeMinBr bs=let h= minimum (map height bs)
             in filter ((==h). height) bs 
			 

minBr :: [Int]->[Branch]
minBr xs = takeMinBr (brs xs)-}