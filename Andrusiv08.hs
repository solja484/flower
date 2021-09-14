{-# OPTIONS_GHC -Wall #-}
module Andrusiv08 where
 
import Data.Char 
import Data.List()

data Recur = Zero | Succ | Sel Int Int 
           | Super Recur [Recur] 
           | Prim Recur Recur 
           | Mini Recur Int 
           | Name String  deriving (Show, Eq)
type System = [(String,Recur)]  


-- Task 1 ------------------------------------

{-“Функція-число” - це унарна (функція від одного аргументу) функція-константи, що завжди обчислює деяке натуральне число.  
•	Zero – “функція-число”, що обчислює 0. 
•	Якщо  f – “функція-число”, що обчислює число n, то  Super Succ [f] – “функція-число”, що обчислює n+1.
•	Якщо  пара  (nm,f)  - елемент множини рекурсивних функцій (System) і  f – “функція-число”, то  (Name nm) – “функція-число”.
•	Інших “функцій-чисел” немає.
-}

isNumbConst :: System -> Recur -> Bool 
isNumbConst _ Zero = True
isNumbConst _ (Super Succ _) = True
isNumbConst syst (Name nm) | containsRecur syst nm = isNumbConst syst (getRecur syst nm)
                           | otherwise = False
isNumbConst _ _ = False

containsRecur:: System -> String -> Bool
containsRecur syst s | null syst = False
                     | fst(head syst)==s = True
                     | otherwise = containsRecur (tail syst) s


-- Task 2 ------------------------------------

{-   Арність (ранг, кількість аргументів) r(f) рекурсивної функції f обчислюється за правилами: 
•	Ранг базових функцій Zero, Succ – дорівнює 1.
•	Ранг базових функцій-селекторів Sel n k – дорівнює n.
•	Ранг результату суперпозиції Super b al  - це ранг одної з функцій-аргументів  r(head al).
•	Результат примітивної рекурсії Prim i st  -  r(st)-1.
•	Результат операції мінімізації Mini b _ - r(b) -1.
•	Ранг Name f – це ранг означення функції r(f).
-}

evRank :: System -> Recur -> Int 
evRank _ Zero =1
evRank _ Succ = 1
evRank _ (Sel n _) = n
evRank syst (Super _ al) = evRank syst (head al)
evRank syst (Prim _ st) = (evRank syst st) -1
evRank syst (Mini b _) = (evRank syst b) -1
evRank syst (Name s)=evRank syst (getRecur syst s)

getRecur::System -> String-> Recur
getRecur syst s | null syst = error "no function with this name in system"
                | fst(head syst)==s = snd (head syst)
                | otherwise = getRecur (tail syst) s

-- Task 3 ------------------------------------
isNames :: System -> Bool 
isNames syst = noRepeats syst && noSubRepeats syst 


noRepeats :: System->Bool
noRepeats syst | null syst = True
               | containsRecur (tail syst) (fst(head syst)) = False
               | otherwise = noRepeats (tail syst)


noSubRepeats :: System->Bool
noSubRepeats syst | null syst = True
                   | (length syst)==1 = and [ not (containsRecur syst r) | r<-getAllNames (snd(head syst))]
                   | noSubRepeatsInLast syst = noSubRepeats (tail syst)
                   | otherwise = False


noSubRepeatsInLast :: System -> Bool
noSubRepeatsInLast syst = and [ (containsRecur (init syst)  r) | r<-(getAllNames recur)]
                   where recur = (snd (last syst))


getAllNames :: Recur ->[String]
getAllNames Zero = []
getAllNames Succ = []
getAllNames (Sel _ _ ) =[]
getAllNames (Prim r1 r2) = getAllNames r1 ++ getAllNames r2
getAllNames (Mini r _) = getAllNames r
getAllNames (Name nm) = [nm]
getAllNames (Super r recurs) = (getAllNames r) ++ concat [getAllNames r2 | r2<-recurs]
-- Task 4 ------------------------------------
isRecur :: System -> Recur -> Bool
isRecur _ Zero = True
isRecur _ Succ = True
isRecur _ (Sel n k) = n>=1 && k>=1 && k<=n
isRecur syst (Super f gn) | (isRecur syst f) && and[isRecur syst g|g<-gn] = 
                                                and[(evRank syst g)==rank|g<-gn] && (length gn == evRank syst1 f)
                          | otherwise = False
                              where rank = evRank syst (head gn)
isRecur syst (Name nm) | containsRecur syst nm = isRecur syst (getRecur syst nm)
                       | otherwise = False
isRecur syst (Mini g _) | isRecur syst g = (evRank syst g) >1
                        | otherwise = False
isRecur syst (Prim r1 r2) | isRecur syst r1 && isRecur syst r2 = calcPrimRank syst r1 r2
                          | otherwise = False


calcPrimRank:: System -> Recur ->Recur -> Bool
calcPrimRank syst g h | evRank syst h > 2 = ((evRank syst h)-2)==(evRank syst g)
                      | otherwise = ((evRank syst h)==2) && ((evRank syst g) == 1)

-- Task 5 ------------------------------------


eval :: System -> Recur -> [Int] -> Int 
eval _ (Zero) _= 0 
eval _ (Succ) xs = (head xs) +1
eval _ (Sel n k) xs = if (k<=n)&&(length xs >= n) then xs!!(k-1) else 0
eval syst (Super f gn) xs = eval syst f [eval syst g xs|g<-gn]
eval syst (Name nm) xs | containsRecur syst nm = eval syst (getRecur syst nm) xs 
                       | otherwise = error "undefined function"
eval syst (Prim r1 r2) xs =  last(fst(until cond  (stepPrim syst r2) config))
                              where config = ((init xs)++[0]++[(eval syst r1 (init xs))],last xs)
eval _ (Mini _ _) _ = 0


cond :: ([Int],Int)->Bool
cond (xs,n) = last(init xs)==n

stepPrim :: System->Recur->([Int],Int)->([Int],Int)
stepPrim syst r (xs,n) = (init(init xs)++[(last (init xs))+1]++[eval syst r xs], n)



-- Task 6 ------------------------------------
evalPart :: System -> Recur -> [Int] -> Maybe Int 
evalPart _ (Zero) _= Just 0 
evalPart _ (Succ) xs = Just ((head xs) +1)

evalPart _ (Sel n k) xs | (k<=n)&&(length xs >= n) = Just (xs!!(k-1)) 
                        | otherwise = Just 0

evalPart syst (Super f gn) xs = evalPart syst f [eval syst g xs|g<-gn]

evalPart syst (Name nm) xs 
                       | containsRecur syst nm = evalPart syst (getRecur syst nm) xs 
                       | otherwise = Nothing

evalPart syst (Prim r1 r2) xs = case evalPart syst r1 (init xs) of 
    Just res -> case fst (until condM (stepPrimM syst r2) (Just (init xs++[0]++[res]),last xs)) of
        Just ys -> Just (last ys)
        Nothing -> Nothing
    Nothing -> Nothing

evalPart syst (Mini g i) vl | null xs = Nothing
                            | otherwise = Just (head xs)
                              where xs = filter (\x -> (eval syst g (vl ++ [x])) == 0) [0..i]

stepPrimM :: System->Recur->(Maybe [Int],Int)->(Maybe [Int],Int)
stepPrimM syst r1 (Just xs,n) = case evalPart syst r1 xs of
                               Just r ->(Just(init(init xs)++[(last (init xs))+1]++[r]), n)
                               Nothing ->(Nothing,n)

stepPrimM _ _ (Nothing,n) = (Nothing, n)

condM :: (Maybe[Int],Int)->Bool
condM (Just xs,n) = last(init xs)==n
condM (Nothing , _) = True

-- Task 7 ------------------------------------

parseRec :: String -> Maybe System 
parseRec str | isNames syst = Just syst
             | otherwise = Nothing 
             where syst = parseSysElems (cleanStr str)

parseSysElems ::String -> System
parseSysElems str =[(parseOperation el) |el<-s]
                   where s = sliceBy (==';') str

--type System = [(String,Recur)]  
-- "const0=z1"
parseOperation :: String -> (String,Recur)
parseOperation str= (head tuple, parseRecur (concat(tail tuple)))
                 where tuple = sliceBy (=='=') str

parseRecur :: String ->Recur
parseRecur str | str=="z1" = Zero
               | str=="a1" = Succ
               | head(str)=='s' && isDigit (str!!1) = (Sel (digitToInt (str!!1)) (digitToInt (str!!2)))
               | head(str)=='(' = parseSuper (tail (init str)) --a1:s33   a1:(a1:z1) 
               | head(str)=='[' = parsePrim (tail (init str))
               | head(str)=='{' = parseMini (tail (init str))
               | otherwise = (Name str)

   --z1 Zero al Succ s11 Sel 1 1 () Super [] Prim {} Mini
parseSuper :: String ->Recur
parseSuper str   | (contains str ':') = 
                         Super (parseRecur (fst tuple)) [parseRecur x|x<-(sliceGlay(sliceBy (==',') (tail (snd tuple))))]
                 | otherwise = Name " "
                 where tuple = splitAt (getIndex str ':') str-- ["a1","s33"]


sliceGlay:: [String]->[String]
sliceGlay (x:xs)   | length xs <1 = (x:xs)
                   | (contains x '(') && (not (contains x ')')) = [(x++","++head xs)]++(tail xs)
                   | otherwise = [x]++(sliceGlay xs)
sliceGlay []=[]

parsePrim:: String ->Recur -----------------s11,(subtract1:s33)
parsePrim str | (contains str ',') = 
                          Prim (parseRecur (fst tuple)) (parseRecur $ tail(snd tuple))
              | otherwise = Name " "
                  where tuple = splitAt (getIndex str ',') str

parseMini:: String ->Recur --subtractionAbs3,100
parseMini str  | (contains str ',') = Mini (parseRecur (fst tuple)) (read (tail(snd tuple))::Int)
               | otherwise = Name " "
                  where tuple = splitAt (getIndex str ',') str

getIndexes:: String->Char->[Int]
getIndexes str ch= [ y | (x, y) <- zip str [0..], x == ch ]

getIndex:: String->Char->Int
getIndex str ch= head[ y | (x, y) <- zip str [0..], x == ch ]

cleanStr :: String -> String
cleanStr str = filter (\x -> (not (isSpace x))) str

contains :: String-> Char ->Bool
contains st c | null st = False
 | (head st)==c = True
 | otherwise = contains (tail st) c

{-data Recur = Zero | Succ | Sel Int Int 
           | Super Recur [Recur] 
           | Prim Recur Recur 
           | Mini Recur Int 
           | Name String  deriving (Show, Eq)
-}

sliceBy :: (Char -> Bool) -> String -> [String]
sliceBy p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : sliceBy p s''
                            where (w, s'') = break p s'

------------------Examples--------------------
syst1, syst2 :: System 
syst1 = [("const0", Zero)   
   , ("const0v2", Super Zero [Sel 2 1])
   , ("const0v3", Super Zero [Sel 3 1])
   , ("const1v2", Super Succ [Super Zero [Sel 2 1]]) 
   , ("const2", Super Succ [Super Succ [Zero]]) 
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ])) 
  , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))  
  , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))  
   , ("subtract1", Prim Zero (Sel 2 1))  
   , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))  
  , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])     
  , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])  
   , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])  
   , ("subtractionPart", Mini (Name "subtractionAbs3") 100)    
   ]
   
  
   
syst2 = [("f1", Super Succ [Zero])
        ,("f2", Super Succ [Name "f2"])
        ]


sysStr1,sysStr2 :: String    
sysStr1 = " const0 = z1; const0v2  = (z1 : s21); const0v3 = (z1:s31);\n\
          \  const1v2 = (a1 : (z1 : s21));  \n\
          \  const2= (a1:(a1:z1)); addition = [s11, (a1:s33)] ;\n\
          \  multiplication = [z1 , (addition: s33,s31)]; \n\
	      \  notSignum = [(a1:z1),(z1:s21)];\n\
		  \  subtract1 = [z1,s21]; subtraction = [s11, (subtract1:s33)];\n\
		  \  subtractionRev = (subtraction : s22, s21);\n\
          \  subtractionAbs = (addition: subtraction, subtractionRev); \n\
          \  subtractionAbs3=(subtractionAbs:s31, (addition:s32,s33))  ;\n \
          \ subtractionPart = {subtractionAbs3, 100 };"
 
 
 
sysStr2 = " f1 = (a1:z1); f2 = (a1, f2);"