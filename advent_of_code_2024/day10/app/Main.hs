module Main where

import Data.Char(digitToInt, intToDigit)
import qualified Data.Set as Set 
import Text.Printf(printf)
import Control.Monad(unless)
import Debug.Trace(trace)

dirs = [(-1,0), (0,1), (1,0), (0,-1)]
goalLevel=9

data Topo = Topo {levels :: [[Int]]
                 ,height::Int
                 ,width::Int
                 ,trailheads::[Coord] -- All the 0s
                 } deriving (Show)

type Coord = (Int, Int) -- deriving (Show)

positions :: Eq a => a -> [a] -> [Int]
positions elt xs = [i | (i,e) <- zip [0..] xs, elt==e]

coords :: Eq a => a -> [[a]] -> [Coord]
coords elt ls = [(i,j) | (i,li) <- zip [0..] ls, j <- (positions elt li)]

parse :: String -> Topo 
parse content = do
    let ls = lines content
        h = length ls 
        w = length $ ls!!0
        lvls = map (map digitToInt) ls 
        zeros = coords 0 lvls 
     in Topo {levels=lvls, height=h, width=w, trailheads=zeros}

accessible :: Topo -> Int -> Coord -> Bool 
accessible Topo{levels=lvls, height=h, width=w} curlvl (i,j) = do
    -- trace ("i="++show i++" j="++show j++" curlvl="++show curlvl) 
    (i >= 0 && j >= 0 && i < h && j<w && lvls!!i!!j == succ curlvl) -- && trace " walkable\n" True)

goals :: Topo -> Coord -> Int -> Set.Set Coord
goals t@Topo{levels=lvls, height=h, width=w} (i,j) curlvl = do
    if lvls!!i!!j == goalLevel
    then Set.singleton (i,j)
    else let access = filter (accessible t curlvl) (map (\(di,dj) -> (i+di,j+dj)) dirs)
            in Set.unions (map (\c -> goals t c (curlvl+1)) access)

part1 :: String -> Int
part1 content = do
    let t = parse content
        scores = map (\c -> length . goals t c $ 0) (trailheads t)
     in sum scores


goals2 :: Topo -> Coord -> Int -> Int
goals2 t@Topo{levels=lvls, height=h, width=w} (i,j) curlvl = do
    if lvls!!i!!j == goalLevel
    then 1
    else let access = filter (accessible t curlvl) (map (\(di,dj) -> (i+di,j+dj)) dirs)
            in sum (map (\c -> goals2 t c (curlvl+1)) access)

part2 :: String -> Int
part2 content = do
    let t = parse content
        scores = map (\c -> goals2 t c $ 0) (trailheads t)
     in sum scores

main :: IO ()
main = do
    putStrLn "Hello!"
    test_input <- readFile "test_input.txt"
    full_input <- readFile "input.txt"
    -- Tests 
    let t = parse test_input
    unless (height t == 8 && width t == 8) (error "Parsing error")
    unless (accessible t 8 (0,1)) (error "Can't access (0,1) from (0,0)")
    unless (length (goals t (0,2) 0) == 5) (error "Should have a score of 5")
    print "Tests passed!"

    -- Part 1
    let p1Test = part1 test_input
    unless (p1Test == 36) (error $ "wrong result for example on part 1: " ++ (show p1Test))
    let p1 = part1 full_input
    printf "Part 1: %d\n" p1

    -- Part 2
    let p2Test = part2 test_input
    unless (p2Test == 81) (error $ "wrong result for example on part 2: " ++ (show p2Test))
    let p2 = part2 full_input :: Int
    printf "Part 2: %d\n" p2