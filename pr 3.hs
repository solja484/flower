{-# OPTIONS_GHC -Wall #-}
module Andrusiv003 where

type Expr = [Term]
type Term = [String]
build:: String ->[Expr]
build ds= concat[ breaks xss | xss<- breaks ds] -- concat(map breaks(breaks ds))

eval:: Expr -> Int -- [["2","34"], ["23","4"]] -> 2*34 + 23*4
eval xs= sum (map evalTerm xs) 

evalTerm :: Term -> Int --["2","34"] = "2"*"34"
evalTerm xs=product (map read xs) --read(head xs)*(evalTerm tail xs)

find:: Int->String->[Expr]
find v ds= filter(\e->eval e==v)(build ds) --filter ((==v).eval)(build ds)

breaks:: [a]->[[[a]]]
breaks []=[]
breaks [v] = [[[v]]]
breaks (x:xs) = ysss1 ++ ysss2
      where xsss = breaks xs 
            ysss1 = map (\(ys:yss)->[x]:(ys:yss)) xsss 
            ysss2 = map (\(ys:yss)->(x:ys):yss) xsss

showE :: Term -> String
showE xs=tail (concat (map ('*':) xs))
showT :: Expr->String
showT xs=tail (concat (map ('+':) (map showE xs)))
findG :: Int->String->[String]
findG v ds= map showT ( find v ds)


gcdg :: Int->Int->Int
gcdg x y | x>y ghcd(x-y) y
 | x<y ghcd x (y-x)
 | otherwise = x

ghcu :: Int->Int->Int
ghcu x y = fst $ until cond step (x,y)
     where cond (u,v) =u == v
           step(u,v)| u>v =(u-v,v)
                    | otherwise = (u,v-u)		   