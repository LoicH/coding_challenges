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

dirs = [(0,1), (0,-1), (-1,0), (1,0)] :: [Coord]

type Graph = M.Map Node [(Node,Cost)]

addNeighbours :: Node -> a -> Graph -> Graph
addNeighbours k@((i,j), d@(di,dj)) _ graph = do 
            let singleton = M.singleton k (if ((i+di,j+dj), d) `M.member` graph then [(((i+di, j+dj), d), Some 1)] else []) 
             in M.unionWith (++) graph singleton 

buildGraph :: [Coord] -> Graph
buildGraph cs = do 
    -- Empty map of all nodes & directions
    let graph = M.fromList [((c,d),[((c,t),Some 90) | t <- turns d]) | c <- cs, d <- dirs]
    -- For each node/direction: add its neighbours: two turns + an eventual forward
    -- M.foldrWithKey : (k -> a -> b -> b) -> b -> Map k a -> b
    -- b = Graph = Map k a 
    -- k = Node
    -- a = [(Coord, Coord, Int)]
        graphWithNeighbours = M.foldrWithKey addNeighbours graph graph 
     in graphWithNeighbours

    {- fold function k(c,d) _ graph = M.unionWith (++) graph (M.singleton k (if c+d `member` graph then [(c+d, d, 1)] else []))
    -}
data Cost = Infinity | Some Int deriving (Show, Eq)
instance Ord Cost where 
    Infinity `compare` Infinity = EQ
    _ `compare` Infinity = LT
    Infinity `compare` _ = GT
    (Some a) `compare` (Some b) = a `compare` b

instance Num Cost where 
    Some a + Some b = Some (a+b)
    _ + _ = Infinity

type Queue = SL.SortedList (Cost, Node)

visit :: Graph -> Queue -> M.Map Node Cost -> Coord -> Cost
visit graph queue dist end = case SL.uncons queue of 
        Nothing -> undefined
        Just (u, us) -> let (d,current) = traceShow (u) u 
                         in if fst current == end
                            then traceShow "end!" d 
                            else let unvisited = S.fromList . SL.fromSortedList . SL.map snd $ us :: S.Set Node
                                     neighbCosts = [(v,c) | (v,c) <- graph!current, v `S.member` unvisited] :: [(Node,Cost)]
                                     alts = M.fromList [(v, d+c) | (v,c) <- neighbCosts] :: M.Map Node Cost
                                     newDist = M.unionWith min dist alts :: M.Map Node Cost
                                     -- For all v in us, dist[v] = newDist!v
                                     newQueue = SL.map (\(_,v) -> (newDist!v, v)) us :: Queue

                                  in visit graph newQueue newDist end

dijkstra :: Graph -> Node -> Coord -> Cost 
dijkstra graph start end = do 
    let nodes = M.keys graph 
        origDist = M.fromList $ zip nodes (repeat Infinity)
        dist = M.adjust (\_ -> Some 0) start origDist :: M.Map Node Cost
        queue = SL.toSortedList [(d,n) | (n,d) <- M.toList dist]
        n = traceShow queue visit graph queue dist end
     in n

                         


part1 :: String -> Int
part1 content = 0

part2 :: String -> Int
part2 content = 0

main :: IO ()
main = do
    putStrLn "Hello!"
    test_input <- readFile "test_input.txt"
    full_input <- readFile "input.txt"
    -- Tests 
    let (coords, start, end) = parse test_input
        graph = buildGraph coords 
    print graph
    print (dijkstra graph start end)
    print "Tests passed!"

    -- Part 1
    let p1Test = part1 test_input
    unless (p1Test == 1) (error $ "wrong result for example on part 1: " ++ (show p1Test))
    let p1 = part1 full_input
    printf "Part 1: %d\n" p1

    -- Part 2
    let p2Test = part2 test_input
    unless (p2Test == 1) (error $ "wrong result for example on part 2: " ++ (show p2Test))
    let p2 = part2 full_input
    printf "Part 2: %d\n" p2