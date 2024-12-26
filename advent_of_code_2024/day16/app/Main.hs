module Main where

import Data.Char(digitToInt, intToDigit)
import qualified Data.Set as S 
import qualified Data.List as L
import qualified Data.Map as M
import Text.Printf(printf)
import Control.Monad(unless)
import Debug.Trace(trace)

type Coord = (Int, Int)

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

parse :: String -> ([Coord], Coord, Coord)
parse content = do 
    let ls = lines content
        walkables = L.foldl (\accu (l,i) -> accu ++ parseLine l i)[] (zip (lines content) [0..])
        s = search ls 'S'
        e = search ls 'E'
     in (walkables, s, e)

turns :: Coord -> [Coord]
turns (di,dj) = case (di,dj) of 
    (0,_) -> [(1,0), (-1,0)]
    (_,0) -> [(0,1), (0,-1)]
    _ -> undefined

dirs = [(0,1), (0,-1), (-1,0), (1,0)] :: [Coord]

type Graph = M.Map (Coord, Coord) [(Coord, Coord,Int)]

addNeighbours :: (Coord, Coord) -> a -> Graph -> Graph
addNeighbours k@((i,j), d@(di,dj)) _ graph = do 
            let singleton = M.singleton k (if ((i+di,j+dj), d) `M.member` graph then [((i+di, j+dj), d, 1)] else []) 
             in M.unionWith (++) graph singleton 

buildGraph :: [Coord] -> Graph
buildGraph cs = do 
    -- Empty map of all nodes & directions
    let graph = M.fromList [((c,d),[(c,t,90) | t <- turns d]) | c <- cs, d <- dirs]
    -- For each node/direction: add its neighbours: two turns + an eventual forward
    -- M.foldrWithKey : (k -> a -> b -> b) -> b -> Map k a -> b
    -- b = Graph = Map k a 
    -- k = (Coord, Coord)
    -- a = [(Coord, Coord, Int)]
        graphWithNeighbours = M.foldrWithKey addNeighbours graph graph 
     in graphWithNeighbours

    {- fold function k(c,d) _ graph = M.unionWith (++) graph (M.singleton k (if c+d `member` graph then [(c+d, d, 1)] else []))
    -}

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