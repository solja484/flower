{-# OPTIONS_GHC -Wall #-}
module Andrusiv008 where 
import Text.ParserCombinators.Parsec
-- data ParsercT s u m a T - монади-трансформер

num:: Parser Int
num = do res <- many1 digit 
         return $ read res

infOp :: String->(a->a->a)->Parser (a->a->a)
infOp x f = do _ <- string x
               return f

paren:: Parser a ->Parser a
paren p = do _ <- string "("
             v<-p
             _<-string ")"
             return v
             

mulop :: Parser (Int->Int->Int)
mulop = try (infOp ":%" mod)
            <|> infOp ":/" div
            <|> infOp "*" (*)

addop:: Parser (Int->Int->Int)
addop = infOp "+" (+) <|> infOp "-" (-)

factor :: Parser Int
factor = num <|> (paren expr)

term, expr :: Parser Int
term = chainl1 factor mulop
expr = chainl1 factor addop

full::Parser Int
full = do v <- expr
          eof
          return v
		  
		  


