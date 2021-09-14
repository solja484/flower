{-# OPTIONS_GHC -Wall #-}
module Andrusiv09 where
import Data.Maybe

-- розглядаємо лише цілі дані: скаляри  і масиви  
--------------------------------------------------------------------
type Id    = String
data Value = I Int | A [(Int, Int)]  deriving (Eq, Show)
data Op    = Add | Minus | Mul | Less | Equal | Index  deriving (Eq, Show)

data Exp = Const Int 
         | Var Id 
         | OpApp Op Exp Exp 
         | Cond Exp Exp Exp 
         | FunApp Id [Exp] 
         deriving (Eq, Show)

data Stmt = Assign Id Exp 
          | AssignA Id Exp Exp 
          | If Exp Stmt Stmt 
          | While Exp Stmt 
          | Call Id [Exp] 
          | Block [VarDef] [Stmt]
          deriving (Eq, Show)

data VarDef  =  Arr Id | Int Id deriving (Eq, Show)

type FunDef  =  (Id, ([VarDef], Exp))
-- функції повертають лише цілі скалярні дані, не використовують глобальні дані (чисті!!)
type ProcDef = (Id, ([VarDef], Stmt))
type Program = ([VarDef], [FunDef], [ProcDef])

type StateP  = [(Id, Value)]  -- стек даних

data Type    = At | It  deriving (Eq, Show)
type FunEnv  = [(Id,[Type])]
type ProcEnv = [(Id,[Type])]
type VarEnv  = [(Id,Type)]

-- Задача 1 ------------------------------------
updateValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
updateValue a b xs | contains a xs = add a b xs
                   | otherwise = xs++[(a,b)]

add :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
add a b xs | fst(head xs) == a = [(a,b)] ++ (tail xs)
           | otherwise = [head xs]++(add a b (tail xs))

contains :: Eq a => a -> [(a,b)] -> Bool
contains a xs | null xs = False
              | fst(head xs)==a = True
              | otherwise = contains a (tail xs)

-- Задача 2 ------------------------------------
--data Value = I Int | A [(Int, Int)] 
updateArray :: Value -> Value -> Value -> Value
updateArray (A xs) (I a) (I b)= A (updateValue a b xs)
updateArray _ _ _ = error "incorrect syntax!"

-- Задача 3 ------------------------------------
-- data Op    = Add | Minus | Mul | Less | Equal | Index 
applyOp :: Op -> Value -> Value -> Value 
applyOp Add (I a) (I b)= I (a+b)
applyOp Minus (I a) (I b) =I (a-b)
applyOp Mul (I a) (I b) =I (a*b)
applyOp Less (I a) (I b) | a<b = (I 1) | otherwise = (I 0)
applyOp Equal (I a) (I b) | a==b = (I 1) | otherwise = (I 0)
applyOp Index (A xs) (I a) | contains a xs = get a xs | otherwise = (I 0)
applyOp _ _ _ = error "incorrect syntax!"

get ::  Int -> [(Int,Int)] -> Value
get a xs | null xs = (I 0)
         | fst(head xs)==a =I( snd (head xs))
         | otherwise = get a (tail xs)
-- Задача 4 ------------------------------------
evExp ::  Exp -> [FunDef] -> StateP -> Value 
evExp (Const c) _ _ = I c
evExp (Var e) dfx (s:st) | fst s == e = snd s
                         | otherwise = evExp (Var e) dfx st
evExp (Var _) _ [] = I 0
evExp (OpApp f a b) dfx st = applyOp f (evExp a dfx st) (evExp b dfx st)
evExp (Cond c a b) dfx st | res==0 = evExp b dfx st
                          | otherwise = evExp a dfx st
                           where (I res) = evExp c dfx st
evExp (FunApp idd x) dfx st = evExp body dfx 
                                    (mergeIdVal (getIds args) [evExp ex dfx st | ex<-x])
                              where (args, body) = getArgs dfx idd

getIds :: [VarDef]->[Id]
getIds ((Arr a):v) = [a]++getIds v
getIds ((Int a):v) = [a]++getIds v
getIds [] = []

getArgs:: [FunDef]->Id->([VarDef], Exp)
getArgs f idd | fst(head f)==idd = snd (head f)
              | otherwise = getArgs (tail f) idd

mergeIdVal:: [Id]->[Value]->StateP
mergeIdVal ids vals | null ids || null vals= []
                    | otherwise = [(head ids,head vals)]
                                ++mergeIdVal (tail ids) (tail vals)


-- Задача 5 ------------------------------------
evStmt :: Stmt -> [FunDef] -> [ProcDef] -> StateP -> StateP
evStmt = undefined

-- Задача 6 ------------------------------------

iswfExp :: Exp -> VarEnv -> FunEnv -> Maybe Type   
iswfExp (Var e) ve _ = getType e ve
iswfExp (FunApp e ex) ve fe | (contains e fe) 
                           && (and [(iswfExp v ve fe) /= Nothing|v<-ex])
                           && (checkElements [fromJust(iswfExp v ve fe)|v<-ex] (getFuncTypes e fe)) = Just It 
                            | otherwise = Nothing
iswfExp (Const _) _ _=Just It
iswfExp (OpApp op e1 e2) ve fe | (iswfExp e1 ve fe == Nothing)||(iswfExp e2 ve fe == Nothing) = Nothing
                               | otherwise = iswfOp op [
                                                       fromJust (iswfExp e1 ve fe),
                                                       fromJust (iswfExp e2 ve fe)
                                                       ]
iswfExp (Cond e1 e2 e3) ve fe |    (iswfExp e1 ve fe == Nothing)
                                 ||(iswfExp e2 ve fe == Nothing)
                                 ||(iswfExp e3 ve fe == Nothing) = Nothing
                              | otherwise = iswfCond   [
                                                       fromJust (iswfExp e1 ve fe),
                                                       fromJust(iswfExp e2 ve fe),
                                                       fromJust(iswfExp e3 ve fe)
                                                       ]

getType ::  Id -> [(Id,Type)] -> Maybe Type
getType a xs | null xs = Nothing
         | fst(head xs)==a =Just( snd (head xs))
         | otherwise = getType a (tail xs)

checkElements:: [Type]->[Type]->Bool
checkElements ee fe | null ee && null fe = True
                    | null fe && not(null ee) = False
                    | null ee && not(null fe) = False
                    | head ee /= head fe = False
                    | otherwise = checkElements (tail ee) (tail fe)
               

getFuncTypes ::  Id -> [(Id,[Type])] -> [Type]
getFuncTypes a xs | null xs = []
         | fst(head xs)==a =snd (head xs)
         | otherwise = getFuncTypes a (tail xs)


-- Задача 7 ------------------------------------

iswfStmt :: Stmt -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfStmt (Assign idd e) ve fe _  | getType idd ve == Nothing = False
                                | (getType idd ve) == (iswfExp e ve fe ) = True
                                | otherwise = False
iswfStmt (AssignA idd e1 e2) ve fe _ | (iswfExp e1 ve fe)==Nothing || (iswfExp e2 ve fe)==Nothing = False
                                    | getType idd ve == Nothing = False
                                    | otherwise = iswfAssignA 
                                                       [
                                                       fromJust(getType idd ve),
                                                       fromJust(iswfExp e1 ve fe),
                                                       fromJust(iswfExp e2 ve fe) 
                                                       ]
iswfStmt (If e s1 s2) ve fe pe = (iswfStmt s1 ve fe pe) 
                              && (iswfStmt s2 ve fe pe) 
                              && (iswfExp e ve fe == (Just It))
iswfStmt (While e st ) ve fe pe = (iswfStmt st ve fe pe)
                               && (iswfExp e ve fe == Just It)
iswfStmt (Call idd ee ) ve fe pe | null (getFuncTypes idd pe) = False
                                | not (and [(iswfExp v ve fe) /= Nothing|v<-ee]) = False
                                | otherwise = (checkElements [fromJust(iswfExp v ve fe)|v<-ee] (getFuncTypes idd pe))
iswfStmt (Block vardef se) ve fe pe = and [iswfStmt s (mergeVe vd ve) fe pe|s<-se]
                                      where vd = createVe vardef



mergeVe :: VarEnv -> VarEnv -> VarEnv
mergeVe ve (v:vd) | contains (fst v) ve = mergeVe ve vd
                  | otherwise = [v] ++ (mergeVe ve vd)
mergeVe ve [] = ve


createVe :: [VarDef]->VarEnv
createVe [] = []
createVe ((Arr v): vardef) = [(v,At)]++createVe vardef
createVe ((Int v): vardef) = [(v,It)]++createVe vardef
-- Задача 8 ------------------------------------

iswfFunDef :: FunDef -> FunEnv -> Bool
iswfFunDef (idd, (vd, ex)) fe = (contains idd fe) 
                             && (iswfExp ex ve fe)== (Just It) 
                             && (checkElements (getFuncTypes idd fe) [(snd v)|v<-ve])
                            where ve = createVe vd


iswfProcDef :: ProcDef -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfProcDef (_, (vd, stmt)) ve fe pe = iswfStmt stmt newve fe pe
                                         where newve = mergeVe (createVe vd) ve

-- Задача 9 ------------------------------------
iswfProgram :: Program -> Bool
iswfProgram (vd, fd, pd) = let ve = createVe vd
                               fe = createFe fd
                               pe = createPe pd
                           in and[iswfFunDef f fe|f<-fd] && and[(iswfProcDef p ve fe pe)|p<-pd]

createPe :: [ProcDef] -> ProcEnv
createPe pd =[(fst p, [ snd v | v<- createVe $ fst(snd p)])|p<-pd]

createFe :: [FunDef] -> FunEnv
createFe fd =[(fst f, [ snd v | v<- createVe $ fst(snd f)])|f<-fd]


--- Допоміжні функції -----------------------------
lookUp :: Eq a => a -> [(a,b)] -> b
-- Передумова: Пара з ключом a є в списку пар abx
lookUp a abx = maybe (error "lookUp") id (lookup a abx) 

-- формує початкове значення змінної
initv :: VarDef -> (Id, Value)
initv (Arr v) = (v, A [])
initv (Int v) = (v, I 0) 

-- Реалізація виконання програми 
evProgram :: Program -> StateP 
evProgram (dvx, dfx, dpx) = 
   let sb = map initv dvx 
       ( _, s) = lookUp "main" dpx      
   in  evStmt s dfx dpx sb   

--  iswfOp o ts - перевіряє коректність типів операндів ts 
--     бінарної операції o і формує тип результату Just t або Nothing  
iswfOp :: Op -> [Type] -> Maybe Type 
iswfOp Add   [It,It] = Just It
iswfOp Minus [It,It] = Just It
iswfOp Mul   [It,It] = Just It
iswfOp Less  [It,It] = Just It
iswfOp Equal [It,It] = Just It
iswfOp Index [At,It] = Just It
iswfOp _      _      = Nothing

--  iswfCond ts - перевіряє коректність  типів операндів ts
--     умовного виразу і формує тип результату Just t або Nothing 
iswfCond :: [Type] -> Maybe Type 
iswfCond [It,It,It] = Just It
iswfCond [It,At,At] = Just At
iswfCond _          = Nothing 

-- iswfAssignA ts перевіряє коректність  типів операндів ts
--   операції присвоювання значення елементу масива 
iswfAssignA :: [Type] -> Bool
iswfAssignA [At,It,It] = True 
iswfAssignA _          = False  

---- Дані для тестування  -----------------------
-- Стан для тестування
sampleState :: StateP
sampleState = [("x",I 5),("y",I 2),("a", A [(2,3),(0,4), (1,2)])]

varEnv :: VarEnv 
varEnv = [("x",It), ("y",It), ("a",At)]

-- Функція максимум двох чисел 
-- func biggest(m,n)= (m<n ? n : m)
biggest :: FunDef
biggest =("biggest",
          ([Int "m", Int "n"], 
           Cond (OpApp  Less (Var "m") (Var "n"))  (Var "n")  (Var "m")                                                                
           )
         )
-- Функція, що обчислює число Фібоначчі
-- func fib(n) = (n<3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
       ([Int "n"], 
        Cond (OpApp Less (Var "n") (Const 3))
             (Const 1)
             (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const 1)])
                        (FunApp "fib" [OpApp Minus (Var "n") (Const 2)]))
       )
      )

-- Функція - сума елементів масиву 0..n ...
-- func sumA(a[],n) = (n<0 ? 0 : a[n] + sumA (a,n-1))
sumA :: FunDef
sumA = ("sumA",
        ([Arr "a", Int "n"],
         Cond (OpApp Less (Var "n") (Const 0)) 
              (Const 0)
              (OpApp Add (OpApp Index (Var "a") (Var "n"))
                         (FunApp "sumA" [Var "a", OpApp Minus (Var "n")(Const 1)])
              )
        )
       )

funEnv :: FunEnv
funEnv = [("biggest",[It,It]),("fib", [It]),("sumA",[At,It])]

-- Приклад оператору - блоку 
sampleBlock :: Stmt 
sampleBlock = Block [Arr "b"]
                 [AssignA "b" (Const 0) (Const 9), AssignA "b" (Const 2) (Const 5),
                  AssignA "b" (Const 3) (Const 7), AssignA "b" (Const 5) (Const 1),
                  Call "sumA1" [Var "b", Const 5]
                 ]

-- Процедура - додавання двох чисел...
-- proc gAdd(x,y) gSum = x + y 
gAdd :: ProcDef
gAdd = ("gAdd", 
        ([Int "x", Int "y"], 
         Assign "gSum" (OpApp Add (Var "x") (Var "y"))
        )
       )

-- Процедура - сума елементів масиву 0..n ...
-- proc sumA1(a[],n) {i;limit;
--      sA=0; i=0; limit=n+1;
--      while (i<limit){sA=sA+a[i]; i=i+1}
--                   }
sumA1 :: ProcDef
sumA1 = ("sumA1",
         ([Arr "a", Int "n"], 
          Block [Int "i", Int "limit"] 
            [Assign "sA" (Const 0), Assign "i" (Const 0),
             Assign "limit" (OpApp Add (Var "n") (Const 1)),
             While (OpApp Less (Var "i") (Var "limit"))
                   (Block [] 
                     [Assign "sA" (OpApp Add (Var "sA")
                                  (OpApp Index (Var "a") (Var "i"))),
                      Assign "i" (OpApp Add (Var "i") (Const 1))
                     ]
                   )
            ]
         )
        )

procEnv :: ProcEnv 
procEnv = [("gAdd",[It,It]),("sumA1",[At,It])]

-- Повні програми
-- gSum; 
-- proc gAdd(x,y) gSum = x + y 
-- proc main() call gAdd(5,10)   
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],Call "gAdd" [Const  5, Const 10]))])

-- sA
-- proc sumA1(a[],n) {i;limit; .... } 
-- proc main() {b[]; b[0]=9; b[2]=5; b[3]=7; b[5]=1;
--                   call sumA1 (b,5)
--             }

pr2 :: Program
pr2 = ([Int "sA"], [], 
       [sumA1, 
        ("main",([], sampleBlock))
       ])
