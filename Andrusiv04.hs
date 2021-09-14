{-# OPTIONS_GHC -Wall #-}
module Andrusiv04 where

import Data.Char(digitToInt, isDigit)

-- Задача 1 -----------------------------------------
analyseG :: String -> Bool 
analyseG st1 = case s st1 of 
      Just st2 -> null st2 
      Nothing  -> False 
 
 
s :: String -> Maybe String


s ('a':st1) = case s st1 of
       Just ('b':st2) ->
           case a st2 of
               Just ('a':st3) -> Just st3
               _             -> Nothing
       _             -> Nothing

s ('b':st1) = if (null st1) then Just "b" else Just st1
s _ = Nothing

a :: String -> Maybe String
a ('b':'a':st1) = case a st1 of
        Just st2 -> 
            case s st2 of
                Just st3 -> Just st3
                _       -> Nothing
        _       -> Nothing
a ('a':st1) = Just st1
a _ = Nothing
 


   
-- Задача 2 ----------------------------------------
balance :: String -> Bool
balance st | (b st)==Nothing = False
 | otherwise = True


b:: String->Maybe String
b ('(':st1) = case b st1 of 
      Just (')':st2) -> b st2 
      _              -> Nothing

b ('[':st1) = case b st1 of 
      Just (']':st2) -> b st2 
      _              -> Nothing

b ('{':st1) = case b st1 of 
      Just ('}':st2) -> b st2 
      _              -> Nothing
b (' ':st1) = b st1

b st=Just st

-- Задача 3 -----------------------------------------
analyseExpr :: String -> Bool 
analyseExpr st1= case ae st1 of
    Just st2 -> null st2
    _        -> False

ae :: String -> Maybe String 
ae st1 = case aa st1 of
     Just st2-> af st2
     Nothing -> Nothing

aa :: String -> Maybe String 
aa ('(':st1) = case ae st1 of
     Just (')':st2) -> Just st2
     _              -> Nothing
aa (d:st1)| isDigit d = Just st1
aa _                  = Nothing

af :: String -> Maybe String 
af (p:st1) | elem p "+*-" = case aa st1 of
         Just st2 -> af st2
         Nothing  -> Nothing
af st1                    = Just st1


-- Задача 4 -----------------------------------------

evalLeft :: String -> Maybe Int
evalLeft st1 = case le st1 of 
   Just (v,st2)| null st2 -> Just v 
   _                      -> Nothing 

le:: String -> Maybe (Int,String)    
le st1 =  case lf st1 of 
     Just (v1,st2) -> la (v1,st2) 
     Nothing       -> Nothing

lf:: String -> Maybe (Int,String)  
lf ('(':st1) = case le st1 of 
     Just (v,(')':st2)) -> Just (v,st2) 
     _                  -> Nothing  

lf (d:st1) | elem d "0123456789" = Just (digitToInt d,st1) 
lf _            = Nothing

la :: (Int,String) -> Maybe (Int,String) 
la (v1,(d:st1))| elem d "+*-" = case lf st1 of  
     Just (v2,st2) -> let v = if d=='+' then v1+v2 else if d=='*' then v1*v2 else v1-v2  
                      in la (v,st2)  
     Nothing       -> Nothing 
la (v1,st1)                  = Just (v1,st1)

-- Задача 5 -----------------------------------------G2
evalRigth :: String -> Maybe Int 
evalRigth st1 = case re st1 of 
   Just (v,st2)| null st2 -> Just v 
   _                      -> Nothing 

re:: String -> Maybe (Int,String)    
re st1 =  case rf st1 of 
     Just (v1,st2) -> ra (v1,st2) 
     Nothing       -> Nothing

rf:: String -> Maybe (Int,String)  
rf ('(':st1) = case re st1 of 
     Just (v,(')':st2)) -> Just (v,st2) 
     _                  -> Nothing  

rf (d:st1) | elem d "0123456789" = Just (digitToInt d,st1) 
rf _            = Nothing

ra :: (Int,String) -> Maybe (Int,String) 
ra (v1,(d:st1))| elem d "+*-" = case re st1 of  
     Just (v2,st2) -> let v = if d=='+' then v1+v2 else if d=='*' then v1*v2 else v1-v2  
                      in ra (v,st2)  
     Nothing       -> Nothing 
ra (v1,st1)                  = Just (v1,st1)


-- Задача 6 -----------------------------------------G3
evalPrior :: String -> Maybe Int 
evalPrior st1 = case pe st1 of 
   Just (v,st2)| null st2 -> Just v 
   _                      -> Nothing 

pe :: String -> Maybe (Int,String)
pe st1 =  case pt st1 of 
     Just (v1,st2) -> pa (v1,st2) 
     Nothing       -> Nothing

pa :: (Int,String) -> Maybe (Int,String) 
pa (v1,d:st1)| elem d "+-" = case pt st1 of  
     Just (v2,st2) -> let v = if d=='+' then v1+v2 else v1-v2  
                      in pa (v,st2)  
     Nothing       -> Nothing 
pa (v1,st1)                  = Just (v1,st1)

pt :: String -> Maybe(Int,String)  
pt st1 =  case pf st1 of 
     Just (v1,st2) -> pb (v1,st2) 
     Nothing       -> Nothing

pb :: (Int,String) -> Maybe (Int,String) 
pb (v1,(d:st1))| elem d "*" = case pf st1 of  
     Just (v2,st2) -> let v = v1*v2  
                      in pb (v,st2)  
     Nothing       -> Nothing 
pb (v1,st1)                  = Just (v1,st1)

pf:: String -> Maybe (Int,String)  
pf ('(':st1) = case pe st1 of 
     Just (v,(')':st2)) -> Just (v,st2) 
     _                  -> Nothing  

pf (d:st1) | elem d "0123456789" = Just (digitToInt d,st1) 
pf _            = Nothing

------------------------------------------------------




------------------------------------------------------------------
match :: Char -> Maybe String -> Maybe String 
match c (Just (t:st)) | c==t = Just st
match _ _                    = Nothing 

inOp :: Char -> Int -> Int -> Int
inOp c = case c of {'+' -> (+); '-' -> (-); '*' -> (*); _ -> undefined}