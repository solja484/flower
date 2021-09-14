{-# OPTIONS_GHC -Wall #-}
module Main where 
import Counting
import System.Environment

{-main ::IO()
main = do fc <- getContents
          putStr (counting fc)-}
main:: IO()
main = do ns<-getArgs
          fc<- if null ns then getContents else readAll ns
          putStr (counting fc)

readAll :: [String]->IO String
readAll [] = return ""
readAll (n:ns)=do cf1<- readFile n
                  cfa <-readAll ns
                  return (cf1 ++"\n"++cfa)

readInt:: IO Integer
readInt = fmap read getLine


readFloat :: IO Float
readFloat = fmap read getLine
{-

main :: IO ()
main = do
    putStr "Type the first number: "
    x <-  getLine
    putStrLn $ show x-}