module Main where

import Data.Char(digitToInt, intToDigit)
import qualified Data.Set as S 
import qualified Data.List as L
import qualified Data.SortedList as SL
import qualified Data.Map as M
import Data.Map((!),(!?))
import Text.Printf(printf)
import Control.Monad(unless)
import Debug.Trace(trace,traceShow)

type Coord = (Int, Int)
type Node = (Coord, Coord)
type Graph a = M.Map Node [(Node, a)]
data Cost = Infinity | Some Int deriving (Show, Eq)
data CostPath = Inf | S Int (S.Set Coord) deriving (Show, Eq)
type Queue a = SL.SortedList (a, Node)

instance Ord Cost where 
    Infinity `compare` Infinity = EQ
    _ `compare` Infinity = LT
    Infinity `compare` _ = GT
    (Some a) `compare` (Some b) = a `compare` b

instance Num Cost where 
    Some a + Some b = Some (a+b)
    _ + _ = Infinity


instance Ord CostPath where 
    Inf `compare` Inf = EQ
    _ `compare` Inf = LT
    Inf `compare` _ = GT
    (S a _) `compare` (S b _) = a `compare` b

instance Num CostPath where 
    S a sa + S b sb = S (a+b) (S.union sa sb)
    _ + _ = Inf

getPath :: CostPath -> S.Set Coord
getPath Inf = undefined
getPath (S _ s) = s

dirs = [(0,1), (0,-1), (-1,0), (1,0)] :: [Coord]

parseLine :: String -> Int -> [Coord] 
parseLine l i = do 
    let walkables = filter (\(c,_) -> c/='#') (zip l [0..])
        coords = [(i,j) | (_,j) <- walkables]
     in coords 

search :: [String] -> Char -> Coord 
search ls c = do 
    let Just i = L.findIndex (elem c) ls
        Just j = L.findIndex ((==) c) (ls!!i)
     in (i,j)

parse :: String -> ([Coord], Node, Coord)
parse content = do 
    let ls = lines content
        walkables = L.foldl (\accu (l,i) -> accu ++ parseLine l i) [] (zip (lines content) [0..])
        s = search ls 'S'
        e = search ls 'E'
     in (walkables, (s,(0,1)), e)

turns :: Coord -> [Coord]
turns (di,dj) = case (di,dj) of 
    (0,_) -> [(1,0), (-1,0)]
    (_,0) -> [(0,1), (0,-1)]
    _ -> undefined


addNeighbours :: b -> Node -> a -> Graph b -> Graph b
addNeighbours c k@((i,j), d@(di,dj)) _ graph = do 
            let singleton = M.singleton k (if ((i+di,j+dj), d) `M.member` graph then [(((i+di, j+dj), d), c)] else []) 
             in M.unionWith (++) singleton graph 

buildGraph :: [Coord] -> a -> a -> Graph a
buildGraph cs costTurn costForward = do 
    -- Empty map of all nodes & directions
    let sortedNodes = SL.toSortedList cs
        graph = M.fromList [((c,d),[((c,t), costTurn) | t@(di,dj) <- turns d, (i+di,j+dj) `SL.elemOrd` sortedNodes]) | c@(i,j) <- cs, d <- dirs]
    -- For each node/direction: add its neighbours: two turns + an eventual forward
        graphWithNeighbours = M.foldrWithKey (addNeighbours costForward) graph graph 
     in graphWithNeighbours



updateQ :: Ord a => M.Map Node a -> M.Map Node a -> Node -> a -> Queue a -> Queue a 
updateQ oldDist newDist n _ q = SL.insert (newDist!n, n) (SL.delete (oldDist!n, n) q)

-- deleteQ :: Queue -> Node -> Cost -> Queue 
-- deleteQ q n c = SL.delete (c,n) q


visit :: Graph Cost -> Queue Cost -> M.Map Node Cost -> Coord -> Cost
visit graph queue dist end = case SL.uncons queue of 
        Nothing -> undefined
        Just (u, us) -> let (d,current) = traceShow (u) u 
                         in if fst current == end
                            then traceShow "end!" d 
                            else let unvisited = S.fromList . SL.fromSortedList . SL.map snd $ us :: S.Set Node
                                     neighbCosts = [(v,c) | (v,c) <- graph!current, v `S.member` unvisited] :: [(Node,Cost)]
                                     alts = M.fromList [(v, d+c) | (v,c) <- neighbCosts] :: M.Map Node Cost
                                     newDist = M.unionWith min dist alts :: M.Map Node Cost
                                     updatedQ = M.foldrWithKey (updateQ dist newDist) us alts :: Queue Cost
                                  in visit graph updatedQ newDist end

keepMin :: CostPath -> CostPath -> CostPath
keepMin a@(S ca sa) b@(S cb sb) 
    | ca == cb = S ca (S.union sa sb)
    | otherwise = min a b 
keepMin a b = min a b 

visit2 :: Graph CostPath -> Queue CostPath -> M.Map Node CostPath -> M.Map Node CostPath
visit2 graph q dist = case SL.uncons q of 
    Nothing -> dist 
    Just ((curCost, curNode), us) -> do 
        let unvisited = traceShow (curNode, curCost) S.fromList . SL.fromSortedList . SL.map snd $ us :: S.Set Node
            alts = M.fromList [(v, curCost+c+ (S 0 $ S.singleton . fst $ v)) | (v,c) <- graph!curNode, v `S.member` unvisited] :: M.Map Node CostPath 
            newDist = traceShow alts M.unionWith keepMin dist alts 
            updatedQ = M.foldrWithKey (updateQ dist newDist) us alts 
         in visit2 graph updatedQ newDist 

dijkstra :: Graph Cost -> Node -> Coord -> Cost 
dijkstra graph start end = do 
    let nodes = M.keys graph 
        origDist = M.fromList $ zip nodes (repeat Infinity)
        dist = M.adjust (\_ -> Some 0) start origDist :: M.Map Node Cost
        queue = SL.toSortedList [(d,n) | (n,d) <- M.toList dist]
        n = traceShow queue visit graph queue dist end
     in n


dijkstra2 :: Graph CostPath -> Node -> Coord -> S.Set Coord
dijkstra2 graph start end = do 
    let nodes = M.keys graph 
        origDist = M.fromList $ zip nodes (repeat Inf)
        dist = M.adjust (\_ -> S 0 S.empty) start origDist :: M.Map Node CostPath
        queue = SL.toSortedList [(d,n) | (n,d) <- M.toList dist]
        endDist = visit2 graph queue dist :: M.Map Node CostPath
        shortestPathEnd = getPath $ L.foldl (\s d -> min s (endDist!(end, d))) Inf dirs 
     in traceShow shortestPathEnd shortestPathEnd

part1 :: String -> Int
part1 content = do 
    let (coords, start, end) = parse content
        graph = buildGraph coords (Some 1000) (Some 1)
        Some n = (dijkstra graph start end)
     in n


part2 :: String -> Int
part2 content = do 
    let (coords, start, end) = parse content
        graph = buildGraph coords (S 1000 S.empty) (S 1 S.empty)
        s = dijkstra2 graph start end
     in S.size s


main :: IO ()
main = do
    putStrLn "Hello!"
    test_input <- readFile "test_input.txt"
    test_input2 <- readFile "test_input2.txt"
    full_input <- readFile "input.txt"
    -- Tests 
    -- let (coords, start, end) = parse test_input
    --     graph = buildGraph coords (S 1000 S.empty) (S 1 S.empty) :: Graph CostPath
    -- print graph
    -- print "Dijkstra:"
    -- print (dijkstra2 graph start end)
    print "Tests passed!"
    -- Part 1
    -- let p1Test = part1 test_input
    -- unless (p1Test == 7036) (error $ "wrong result for example on part 1: " ++ (show p1Test))
    -- let p1 = part1 full_input
    -- printf "Part 1: %d\n" p1

    -- Part 2
    let p2Test = part2 test_input
    unless (p2Test == 45) (error $ "wrong result for example on part 2: " ++ (show p2Test))
    let p2Test2 = part2 test_input2
    unless (p2Test2 == 64) (error $ "wrong result for example on part 2: " ++ (show p2Test2))
    let p2 = part2 full_input
    printf "Part 2: %d\n" p2