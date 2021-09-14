{-# OPTIONS_GHC -Wall #-}
module Counting where 
counting::String->String
counting str= show (length ls, length ws, cc)
              where ls=lines str
                    ws=concatMap words ls
                    cc = sum $ map length ws

