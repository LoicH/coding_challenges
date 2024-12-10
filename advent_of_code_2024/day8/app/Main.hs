
module Main where

import Data.List as List
import Data.Map as Map 
import Data.Set as Set 
import Text.Printf(printf)
import Control.Monad 
import Debug.Trace


type Coord = (Int, Int)

data City = City {antennas:: Map.Map Char [Coord]
                , height:: Int
                , width:: Int
                } deriving (Show)

parse :: String -> City
parse content = 
    let ls = lines content 
        h = length ls 
        w = length (ls !! 0)
        as = List.foldl (Map.unionWith (++)) Map.empty (List.map (\i -> parseLine (ls!!i) i) [0..h-1])
    in City {antennas=as, height=h, width=w}

-- "..a..a.A" -> [('a', 2), ('a', 5), ('A', 7)]
parseLine :: String -> Int -> Map.Map Char [Coord]
parseLine l i = Map.fromListWith (++) [(l!!j, [(i,j)]) | j<-[0..length l-1], l!!j /= '.']

antinodes :: (Coord,Coord) -> (Coord, Coord)
antinodes ((xa,ya),(xb,yb)) = ((2*xa-xb, 2*ya-yb), (2*xb-xa, 2*yb-ya))

flatten :: [[a]] -> [a]
flatten xs = List.foldl (++) [] xs

flattenTuples :: [(a,a)] -> [a]
flattenTuples [] = []
flattenTuples ((a,b):xs) = a:b:(flattenTuples xs)

inMap :: Int -> Int -> Coord -> Bool
inMap h w (x,y) = x >= 0 && y >= 0 && x < h && y < w

antinodesGroup :: Int -> Int -> [Coord] -> Set Coord
antinodesGroup h w xs = 
    let couples = List.map antinodes (combinations xs)
        anodes = Set.fromList (flattenTuples couples)
        valids = Set.filter (inMap h w) anodes 
    in valids

combinations :: [a] -> [(a,a)]
combinations [] = []
combinations [x] = []
combinations (x:xs) = [(x,a) | a <- xs] ++ combinations xs

part1 :: String -> Int
part1 content = 
    let city = parse content
        h = height city
        w = width city
        as = antennas city
        allAntinodes = List.map (antinodesGroup h w) [snd x | x<-Map.toList as]
        unionAntinodes = List.foldl Set.union Set.empty allAntinodes
    in length unionAntinodes

-- Part 2
antinodes2 :: Int -> Int -> (Coord, Coord) -> [Coord]
antinodes2 h w ((xa,ya), (xb,yb)) = 
    let (dx, dy) = (xb-xa, yb-ya)
        fromB = takeWhile (inMap h w) [(xb+k*dx, yb+k*dy) | k <- [0..]]
        fromA = takeWhile (inMap h w) [(xa-k*dx, ya-k*dy) | k <- [0..]]
    in fromA ++ fromB


antinodesGroup2 :: Int -> Int -> [Coord] -> Set Coord
antinodesGroup2 h w xs = 
    let anodes = List.map (antinodes2 h w) (combinations xs)
        uniques = Set.fromList (flatten anodes)
    in uniques

part2 :: String -> Int
part2 content = 
    let city = parse content
        h = height city
        w = width city
        as = antennas city
        allAntinodes = List.map (antinodesGroup2 h w) [snd x | x<-Map.toList as]
        unionAntinodes = List.foldl Set.union Set.empty allAntinodes
    in length unionAntinodes

main :: IO ()
main = do
    test_input <- readFile "test_input.txt"
    -- Tests 
    let city = parse test_input
    print "City:"
    print city
    let h = height city
        w = width city 
        Just as = Map.lookup 'A' (antennas city) 
    print "Antennas 'A':"
    print as
    print "Antinodes:"
    print (antinodesGroup h w as)
    let allAntinodes = List.map (antinodesGroup h w) [snd x | x<-Map.toList (antennas city)]
    print "All antinodes:"
    print allAntinodes
    let unionAntinodes = List.foldl Set.union Set.empty allAntinodes
    print "union antinodes:"
    print unionAntinodes
    -- Part 1
    let p1Test = part1 test_input
    unless (p1Test == 14) (error $ "wrong result for example on part 1: " ++ (show p1Test))
    full_input <- readFile "input.txt"
    let p1 = part1 full_input
    printf "Part 1: %d\n" p1

    -- Part 2
    let p2Test = part2 test_input
    unless (p2Test == 34) (error $ "wrong result for example on part 2: " ++ (show p2Test))
    let p2 = part2 full_input :: Int
    printf "Part 2: %d\n" p2
