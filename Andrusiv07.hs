{-# OPTIONS_GHC -Wall #-}
module HWP07 where
import Data.List

data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq) 
--- B-дерево порядка t (NodeB kl tl) =>  
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
data Btree a =  NodeB [a] [Btree a]  deriving (Show, Eq)
-- -- головні характеристики B-дерево  (BInform heigth min max)
data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)



-- Task 1 ------------------------------------
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True
isSearch (NodeM a n left right ) | n<=0 = False
                                 | left==EmptyM && right==EmptyM = True
                                 | left==EmptyM = (getA right)>a && isSearch right && smallerThan a r
                                 | right==EmptyM = (getA left)<a && isSearch left && biggerThan a l
                                 | otherwise = and[(getA right)>a,(getA left)<a, isSearch right , isSearch left, smallerThan a r, biggerThan a l]
                                 where l = elementsList left
                                       r = elementsList right


getA::  (Ord a) => BinTreeM a -> a
getA (NodeM a _ _ _) = a
getA EmptyM = error "Unexpected result"


-- less
biggerThan :: (Ord a) => a -> [a] -> Bool
biggerThan _ [] = True
biggerThan a (x:xs) =  if (a<=x )then False else (biggerThan a xs)


smallerThan :: (Ord a) => a -> [a] -> Bool
smallerThan _ [] = True
smallerThan a (x:xs) = if (a>=x) then False else (smallerThan a xs)


elementsList :: (Ord a) => BinTreeM a -> [a]
elementsList EmptyM  = []
elementsList (NodeM a _ left right) | left==EmptyM && right==EmptyM = [a]
                                    | right==EmptyM =  elementsList left ++ [a]
                                    | left==EmptyM =[a] ++ elementsList right 
                                    | otherwise = elementsList left ++ [a] ++ elementsList right

-- Task 2 ------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch (NodeM a _ left right) el | el==a = True
                                     | el<a = elemSearch left el
                                     | otherwise = elemSearch right el

-- Task 3 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insSearch EmptyM v=NodeM v 1 EmptyM EmptyM
insSearch (NodeM a n left right ) v | a==v = NodeM a (n+1) left right 
                                   | v < a = NodeM a n (insSearch left v) right
                                   | otherwise = NodeM a n left (insSearch right v)


-- Task 4 ------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
delSearch EmptyM _ = EmptyM
delSearch (NodeM a n left right) v | a==v&&n>1 = (NodeM a (n-1) left right)
                                   | a==v&&n==1 = deleteRoot (NodeM a n left right)
                                   | v < a = (NodeM a n (delSearch left v) right)
                                   | otherwise = (NodeM a n left (delSearch right v))
                       
deleteRoot :: (Ord a) => BinTreeM a -> BinTreeM a
deleteRoot EmptyM = error "Can't delete root in empty tree!"
deleteRoot (NodeM _ _ left right) 
    | left==EmptyM = right
    | right==EmptyM = left
    | otherwise = NodeM newRoot newN left (delSearch right newRoot)
                  where newRoot = firstLeftElement right
                        newN = elemNumber right newRoot

firstLeftElement :: (Ord a) => BinTreeM a -> a
firstLeftElement EmptyM = error "Empty tree in rightTreeLeft"
firstLeftElement (NodeM v _ EmptyM _) = v
firstLeftElement (NodeM _ _ lt _) = firstLeftElement lt

elemNumber :: (Ord a) => BinTreeM a -> a -> Int
elemNumber EmptyM _ = error "Empty tree in elemNumber!"
elemNumber (NodeM a n left right) v | v==a = n
                                    | v<a = elemNumber left v
                                    | otherwise = elemNumber right v

-- Task 5 ------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList a = treeToList (foldl (insSearch) EmptyM a)

treeToList :: (Ord a) => BinTreeM a -> [a]
treeToList EmptyM = []
treeToList (NodeM v n lt rt) = treeToList(lt) ++ [v| _<-[1..n]] ++ treeToList(rt)


-- Task 6 ------------------------------------

findBInform :: (Bounded a, Ord a) => Btree a ->  BInform a
findBInform bt = BInform { hB=findHeight bt , minB= findMin bt , maxB=findMax bt }

findHeight :: (Bounded a, Ord a) => Btree a -> Int 
findHeight (NodeB _ childs) | null childs = 0 
                            | otherwise = 1 + findHeight (head childs)


findMin :: (Bounded a, Ord a) => Btree a -> a
findMin (NodeB a childs) | null childs = head a
                         | otherwise = findMin (head childs)

findMax :: (Bounded a, Ord a) => Btree a -> a
findMax (NodeB a childs) | null childs = last a
                         | otherwise = findMax (last childs)

isLeaf :: (Bounded a, Ord a) => Btree a -> Bool
isLeaf (NodeB _ childs) = null childs
-- Task 7 ------------------------------------

{-
tBt1 = NodeB "L"
       [ NodeB "DG" 
          [ NodeB "AC" [], NodeB "EE" [], NodeB "HK" []
          ]
       , NodeB "PU" 
          [ NodeB "MM" [], NodeB "RS" [], NodeB "UW" []
          ]
       ]
-}

-- Btree Node "elem" [childs]
-- Btree a =  NodeB [a] [Btree a]  deriving (Show, Eq)
-- data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)

isBtree  :: (Bounded a, Ord a) => Int -> Btree a -> Bool 
isBtree _ (NodeB [] _ )= False
isBtree t bt=and[checkHeight bt, checkKeys t bt, checkRoot t bt, checkSons bt,checkVert bt, checkHor bt]

--Ключі в кожному вузлі впорядковані по неспаданню
checkVert :: (Bounded a, Ord a) => Btree a -> Bool 
checkVert (NodeB a childs) | null childs = True
                           | null a = True
                           | otherwise = and[head a >= x | x<-getElements (head childs)]
                             && checkVert (NodeB (tail a) (tail childs))
                             && and[checkVert ch|ch<-childs]
                             && and[head a <= x | x<-getElements (last childs)]

checkHor :: (Bounded a, Ord a) => Btree a -> Bool 
checkHor (NodeB a childs) | null childs = True
                          | otherwise =  a==sortElements a
                                         &&tbt == sortElements tbt
                                         && and [checkHor chh| chh<-childs]
                                         where tbt = concat[getElements ch|ch<-childs]

-- Якщо дерево не порожнє, то корінь має n ключів 1 <= n <= (2*t-1) 
checkRoot :: (Bounded a, Ord a) => Int -> Btree a -> Bool
checkRoot t (NodeB a []) = (length a >= 1)&& (length a <=(2*t-1))
-- корінь має (n+1) сина, якщо висота дерева більше 0.
checkRoot t (NodeB a childs) = (length a >= 1)&& (length a <=(2*t-1))&& (length childs == (length a + 1))

-- Внутрішній вузол (не листок) має  (n+1) сина.
checkSons :: (Bounded a, Ord a) =>  Btree a -> Bool
checkSons (NodeB a childs) | null childs = True 
                           | otherwise = (((length a) + 1)==length childs)
                                         && (and [checkSons ch| ch<-childs])

-- Кожний вузол, крім корeня, має  n ключів (t-1) <= n <= (2*t-1) 
-- 2<=n<=5 //3
-- 1<=n<=3 //2
checkKeys :: (Bounded a, Ord a) => Int -> Btree a -> Bool
checkKeys t (NodeB a childs) | null childs = True
                             | otherwise = (length ch>=t-1)
                                           &&(length ch<=(2*t-1))
                                           && checkKeys t (NodeB a (tail childs)) 
                                           && checkKeys t (head childs)
                              where ch = getElements (head childs)

--	Всі листки знаходяться на одному рівні.    
checkHeight:: (Bounded a, Ord a) => Btree a ->Bool
checkHeight bt = and[findHeight bt==ch|ch<-allHeights bt]

allHeights:: (Bounded a, Ord a) => Btree a ->[Int]
allHeights (NodeB a childs) | null childs = []
                            | otherwise = [findHeight (NodeB a childs)] ++ allHeights (NodeB a (tail childs))

-- Task 8 ------------------------------------
eqBtree :: (Bounded a, Ord a) => Int -> Btree a -> Btree a -> Bool
eqBtree _ (NodeB a childs1) (NodeB b childs2) = sortElements(a ++ elements childs1) == sortElements(b ++ elements childs2) 

sortElements ::(Bounded a, Ord a) => [a] -> [a]
sortElements xs = sortBy sortGT xs

getElements::(Bounded a, Ord a) => Btree a -> [a]
getElements (NodeB a _) = a
getChilds::(Bounded a, Ord a) => Btree a -> [Btree a]
getChilds (NodeB _ childs) = childs

elements :: (Bounded a, Ord a) => [Btree a] -> [a]
elements childs | null childs = []
                | null (tail childs) =  getElements (head childs) ++ elements (getChilds (head childs))
                | otherwise = getElements (head childs) ++ elements (getChilds (head childs)) ++ elements (tail childs)

sortGT :: Ord a => a -> a -> Ordering
sortGT a1 a2
  | a1 < a2 = LT
  | a1 >= a2 = GT
sortGT _ _ = error " "
-- Task 9 ------------------------------------
elemBtree :: (Bounded a, Ord a) => Btree a -> a -> Bool
elemBtree (NodeB a childs) v = elem v (sortElements (a ++ elements childs))


-- Task 10 ------------------------------------


insBtree :: Ord a => Int -> Btree a -> a -> Btree a
insBtree t (NodeB el tree) v 
 | isFull t (NodeB el tree) = let (tr1,km, tr2) = splitAtB t (NodeB el tree) 
                              in splitTemp t (NodeB [km] [tr1,tr2]) v 
 | otherwise = splitTemp t (NodeB el tree) v 

isFull :: Ord a => Int -> Btree a -> Bool
isFull t (NodeB a _) = (2*t-1) == (length a)  

insertKey :: Ord a => a -> [a] -> [a]
insertKey v (x:xs) | v < x = v:(x:xs)
                   | otherwise = x:(insertKey v xs)
insertKey v [] = [v]    


findPos :: Ord a => a -> [(a,Int)] -> Maybe Int
findPos v xs
 | v <=(fst (head xs)) = Just (snd (head xs))
 | otherwise = findPos v ( tail xs) 

position :: Ord a => a -> [a] -> Int
position v xs= case findPos v (zip xs [0..]) of
                    Just x -> x
                    Nothing -> length xs

splitTemp::  Ord a => Int-> Btree a->a-> Btree a
splitTemp t (NodeB el tree) v 
 | null tree = (NodeB (insertKey v  el)  tree) 
 | otherwise = if isFull t (tree!!pos) then
  let (tr1,km,tr2) = splitAtB t (tree!!pos) 
  in splitTemp t (NodeB (insertKey km el) ((take pos tree) ++ [tr1,tr2] ++ (drop (pos+1) tree))) v 
  else (NodeB el ((take pos tree) ++ [splitTemp t (tree!!pos) v] ++ (drop (pos+1) tree)))
  where  pos = (position v el)


splitAtB :: Ord a => Int -> Btree a -> (Btree a, a, Btree a)
splitAtB t (NodeB a childs)= let km = a !! (div t 2)
                                 (k1, k2) = splitAt (div t 2) a
                                 (tr1, tr2) = splitAt ((+) (div t 2) 1) childs
                             in ((NodeB k1 tr1), km, (NodeB (tail k2) tr2)) 
      




-----------Тестові дані - Дерева пошуку -------
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   

{-
tiB1 :: Btree Char 
tiB1 = NodeB ['G','M','P','X'] 
             [ NodeB ['A','C','D','E'] []
             , NodeB ['J','K'] []
             , NodeB ['N','O'] []
             , NodeB ['R','S','T','U','V'] []
             , NodeB ['Y','Z'] [] ]

tBtr1 :: Btree Int
tBtr1 = NodeB [5,10,12] [ts0,ts1,ts2,ts3]
   where ts0 = NodeB [1,3  ] []   --- ,4,5] []  --
         ts1 = NodeB [6,6 ,8,9,10] [] --- ,8,9,10] []  -- ] []   
         ts2 = NodeB [11,11,12,12] []
         ts3 = NodeB [16,16] [] -- ,18,19,20] [] 

tBtr2 :: Btree Int 
tBtr2 = NodeB [15] [ts10,ts11]
  where ts10 = NodeB [11,13] [] 
        ts11 = NodeB [21,22] []  
-}
tBt1 :: Btree Char 
tBt1 = NodeB "L"
       [ NodeB "DG" 
          [ NodeB "AC" [], NodeB "EE" [], NodeB "HK" []
          ]
       , NodeB "PU" 
          [ NodeB "MM" [], NodeB "RS" [], NodeB "UW" []
          ]
       ]

tBt2 :: Btree Char 
tBt2 = NodeB "GP"
       [ NodeB "ACDEE" [], NodeB "HKLMM" [], NodeB "RSUUW" []
       ]



tBt5 :: Btree Char 
tBt5 = NodeB "GMPX"
       [ NodeB "ACDE" [] , NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt6 :: Btree Char 
tBt6 = NodeB "GMPX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt7 :: Btree Char 
tBt7 = NodeB "GMPTX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
       ]

tBt8 :: Btree Char 
tBt8 = NodeB "P"
       [ NodeB "GM"
          [ NodeB "ABCDE" [], NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]

tBt9 :: Btree Char 
tBt9 = NodeB "P"
       [ NodeB "CGM"
          [ NodeB "AB" [], NodeB "DEF" []
          , NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]
