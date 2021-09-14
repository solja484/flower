{-# OPTIONS_GHC -Wall #-}
module Andrusiv004 where
c,b::String ->Maybe String
c('d':st1)=case c st1 of --C->dCd
    Just ('d':st2)->Just st2
    _             ->Nothing

c('a':st1)=case b st1 of --C->aBa
    Just ('a':st2)->Just st2
    _             ->Nothing
c _       =Nothing --error


b('e':st1)=b st1 --B->eB
b st1     =Just st1 --B->e


analyseG :: String->Bool
analyseG st1=case c st1 of
   Just st2 ->null st2
   Nothing  ->False
   
   
   
s:: String->Maybe String
s('(':st1) = case s st1 of --S-> (S)S
   Just (')':st2)->s st2
   _             ->Nothing
s st       = Just st  --S->e
