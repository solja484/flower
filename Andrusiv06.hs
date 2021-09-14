{-# OPTIONS_GHC -Wall #-}
module Andrusiv06 where

type GraphS = (Int,[(Int,Int)])            
type Graph  = [[Int]]

edges::Graph->[(Int,Int)] --всі ребра графа
edges g=[(x,y)|x<-nodes g,y<-g!!x]

nodes::Graph->[Int] --всі вершини графа
nodes g=[0..((length g)-1)]

-- Task 1 ------------------------------------
isOrdinary :: Graph -> Bool 
isOrdinary gr | haveDuplicates xs = False
              | otherwise = and [(elem (snd x, fst x) xs)&&(snd x/=fst x)| x <- xs]
                where xs = edges gr

haveDuplicates:: [(Int,Int)]->Bool
haveDuplicates e | (length e)==1 = False
                 | (e!!0) == (e!!1) = True
                 | otherwise = haveDuplicates (tail e)

-- Task 2 ------------------------------------
fromGraph :: Graph -> GraphS 
fromGraph g=((length g) -1, [(n,x)|n<-(nodes g), x<-g!!n]) 


-- Task 3 --------------------------------------(Int,[(Int,Int)])  ->  [[Int]]
toGraph :: GraphS -> Graph 
toGraph gs= toGraph1 (snd gs) (emptyList (fst gs))

toGraph1 :: [(Int,Int)] -> [[Int]]->Graph
toGraph1 gs g
 | null gs = g
 | otherwise = toGraph1 (tail gs) gr
               where gr | n==0 = [(g!!n)++[snd(head gs)]]++(drop (n+1) g)
                        | n==(length g) = ((take n g) ++ [(g!!(n)++[snd(head gs)])])
                        | otherwise = ((take n g) ++ [(g!!n)++[snd(head gs)]])++(drop (n+1) g)
                     n = fst(head gs) 

emptyList::Int->[[Int]]
emptyList n = [[]|_<-[0..n]]

-- Task 4 ------------------------------------
shortWay :: Graph -> Int -> Int -> [Int] 
shortWay gr a b | elem b (gr!!a) = [a,b]
 | a==b = [a]
 | null ways = []
 | otherwise = (snd $ minimum $ map (\x -> (length x, x)) ways)
               where ways = allWays a b (edges gr)


allWays :: Int -> Int -> [(Int, Int)] -> [[Int]] 
allWays a b egs 
    | a == b = [[a]]
    | otherwise =   [ a:path | 
                            edge <- egs, 
                            a == (fst edge),
                            path <- (allWays (snd edge) b (lastEdges egs edge))
                    ]

lastEdges:: [(Int,Int)]->(Int,Int)-> [(Int,Int)]
lastEdges egs edge= [e | e <- egs, e /= edge]

-- Task 5 ------------------------------------
isConnecting :: Graph -> Bool 
isConnecting gr | (isOrdinary gr) = and [haveWay gr x y|x <- xs, y<-xs]         
                | otherwise = False
                                    where xs = nodes gr

haveWay:: Graph->Int->Int->Bool
haveWay gr x y = not((null (allWays x y (edges gr)))&&(x/=y))

-- Task 6 ------------------------------------
components :: Graph -> [[Int]] 
components gr
 | isConnecting gr = [nodes gr] 
 | otherwise = findComponents gr (nodes gr) [[]]

findComponents :: Graph -> [Int]->[[Int]]->[[Int]] 
findComponents gr nod res 
 | null nod = tail res
 | otherwise = 
        findComponents gr [ y | y<-nod, not( haveWay gr (head nod) y)] 
                            (res ++ [[y| y<-nod, haveWay gr (head nod) y]])

-- Task 7 ------------------------------------
eccentricity :: Graph -> Int -> Int
eccentricity gr v 
 | gr==[[]] = 0
 | isConnecting gr = maximum [length (shortWay gr g v)-1 |g<-nodes gr]
 | otherwise = -1

-- Task 8 ------------------------------------
findDiameter :: Graph -> Int 
findDiameter gr= maximum [ eccentricity gr v |v<-nodes gr]

findRadius :: Graph -> Int 
findRadius gr= minimum (filter (>=0)[eccentricity gr v |v<-nodes gr])

-- Task 9 ------------------------------------
findCenter :: Graph -> [Int] 
findCenter gr
 | isConnecting gr = [v|v<-nodes gr, (eccentricity gr v)==findRadius gr]
 | otherwise = []
-- Task 10 ------------------------------------
shortWays :: Graph -> Int ->Int -> [[Int]] 
shortWays gr a b | a==b  =[] 
                 | otherwise = filter (\x -> length x == (minLength ways)) ways
                               where ways = allWays a b (edges gr)

minLength:: [[Int]] ->Int
minLength gr 
 | length gr== 1 = a 
 | otherwise     = min a (minLength (tail gr))
                 where a = length(head gr)




{---------------------������ ���� - ����� --------}
gr1S, gr2S, gr3S:: GraphS
gr1S = (5,[(0,1),(0,2),(0,3),(1,0),(1,3),(1,4),
           (2,0),(2,4),(2,5),(3,0),(3,1),(4,1),(4,2),(5,2)])
gr2S = (7,[(0,1),(0,3),(1,0),(1,2),(2,1),(2,3),(3,0),(3,2),
           (4,5),(4,6),(5,4),(5,6), (6,4),(6,5)])
gr3S = (3,[(0,1),(0,2),(1,0),(1,3),(2,0),(3,1)])
 
 
gr1, gr2, gr3:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]
gr3 = [[1,2],[0,3],[0],[1]]