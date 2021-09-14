{-# OPTIONS_GHC -Wall #-}
module Andrusiv005 where

putN :: Int->String->IO()
putN n str = if (n<=1) then (putStrLn str) else do putStrLn str
                                                   putStrLn str


firstN::IO()
firstN = do putStr "file>"
            name <- getLine 
            putStr "N>"
            ns <- getLine
            file <- readFile name
            putStr (takeN file ns)

takeN ::String -> String -> String
takeN file ns = unlines $ take(read ns)(lines file) 

lastN ::IO()
lastN = do putStr "file>"
           name <- getLine 
           putStr "N>"
           ns <- getLine
           file <- readFile name
           putStr (takeLN file ns)

takeLN ::String -> String -> String
takeLN file ns = (unlines.reverse.((take.read) ns). reverse .lines) file