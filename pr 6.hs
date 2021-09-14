{-# OPTIONS_GHC -Wall #-}
module Andrusiv006 where


sorting:: Ord a=>[a]->[a]
sorting [] =[]
sorting (x:xs) = insert x (sorting xs)

insert:: Ord a=>a->[a]->[a]
insert a []=[a]
insert a (x:xs) | a<x = a:(x:xs)
                | otherwise =  [x]++(insert a xs)
