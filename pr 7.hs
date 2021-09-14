{-# OPTIONS_GHC -Wall #-}
module Andrusiv007 where 
data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq) 

bt::BinTreeM Char
bt = NodeM 'b' 2 
           (NodeM 'a' 1 EmptyM EmptyM)
           (NodeM 'c' 2 EmptyM EmptyM)
		   
		   
h,d::BinTreeM a->Int
h = fst . hd
d = snd . hd


hd::BinTreeM a->(Int,Int)
hd emptyM=(0,0)
hd (NodeM node num l r)=let (hl,dl) =hd l
                            (hr,dr) =hd r
                        in ((max hl hr)+1,maximum [dl,dr,abs(hl-hr)])




-- B-������ ������� t (NodeB kl tl) =>  
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
data Btree a =  NodeB [a] [Btree a]  deriving (Show, Eq)


---------------(BInform heigth min max)--------
data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)

-- Task 1 ------------------------------------
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch  = undefined

-- Task 2 ------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch = undefined

-- Task 3 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insSearch = undefined

-- Task 4 ------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
delSearch = undefined

-- Task 5 ------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList = undefined

-- Task 6 ------------------------------------
findBInform :: (Bounded a, Ord a) => Btree a ->  BInform a
findBInform = undefined

-- Task 7 ------------------------------------
isBtree  :: (Bounded a, Ord a) => Int -> Btree a -> Bool 
isBtree = undefined

-- Task 8 ------------------------------------
eqBtree :: (Bounded a, Ord a) => Int -> Btree a -> Btree a -> Bool 
eqBtree = undefined

-- Task 9 ------------------------------------
elemBtree :: Ord a => Btree a -> a -> Bool
elemBtree = undefined

position :: Ord a => a -> [a] -> Int
position = undefined

-- Task 10 ------------------------------------
insBtree :: Ord a => Int -> Btree a -> a -> Btree a
insBtree = undefined

isFull :: Ord a => Int -> Btree a -> Bool
isFull = undefined

insertKey :: Ord a => a -> [a] -> [a]
insertKey = undefined

decomposeNodeB :: Ord a => a -> [a] -> [Btree a] -> 
                        ([a], [a], [Btree a], Btree a, [Btree a])
decomposeNodeB = undefined

splitAtB :: Ord a => Int -> Btree a -> (Btree a, a, Btree a)
splitAtB = undefined

------------------------------------------
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