module Main where

import Data.Char(digitToInt, intToDigit)
import qualified Data.Set as Set 
import qualified Data.List as List 
import qualified Data.Map as Map 
import Text.Printf(printf)
import Control.Monad(unless)
import Debug.Trace(trace)

type Coord = (Int, Int) -- deriving (Show)
type Areas = Map.Map Coord Int

dirs = [(-1,0), (0,1), (1,0), (0,-1)]

tupleAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
tupleAdd (a,b) (c,d) = (a+c, b+d)

parse :: String -> [[Char]]
parse = lines 

inMap :: Coord -> [[Char]] -> Bool 
inMap (i,j) garden = let h = length garden 
                         w = length $ garden!!0
                         inbound = i >= 0 && i < h && j >= 0 && j < w 
                    in inbound

dfs :: [Coord] -> [[Char]] -> Char -> Int -> Areas -> Areas
dfs [] _ _ _ areas = areas 
dfs ((i,j):xs) garden c n areas = 
    if {- trace ("i="++show i++", j="++show j++"c="++ show c) -}  (not (inMap (i,j) garden)) || Map.member (i,j) areas || garden!!i!!j /= c 
    then {- trace "skip" -} dfs xs garden c n areas 
    else {- trace "else" -} (let nextSteps = map (tupleAdd (i,j)) dirs 
                                 areasNew = {- trace "insert 4 next steps" -} Map.insert (i,j) n areas 
                                in dfs (nextSteps ++ xs) garden c n areasNew)

splitAreas :: [Coord] -> [[Char]] -> Int -> Areas -> Areas
splitAreas [] _ _ areas = areas 
splitAreas ((i,j):cs) garden n areas = do
    if Map.member (i,j) areas 
    then splitAreas cs garden n areas 
    else let areasNew = dfs [(i,j)] garden (garden!!i!!j) n areas  
           in splitAreas cs garden (n+1) areasNew 

part1 :: String -> Int
part1 content = let garden = parse content 
                    h = length garden 
                    w = length $ garden!!0
                    coords = [(i,j) | i <- [0..h-1], j <- [0..w-1]]
                 in length coords



part2 :: String -> Int
part2 content = 0

main :: IO ()
main = do
    putStrLn "Hello!"
    test_input <- readFile "test_input.txt"
    full_input <- readFile "input.txt"
    -- Tests 
    let garden = parse "OOO\nXOX" 
        h = length garden 
        w = length $ garden!!0
        coords = [(i,j) | i <- [0..h-1], j <- [0..w-1]]
    unless (4 == length (dfs [(0,0)] garden (garden!!0!!0) 0 Map.empty)) (error $ "Should have 4 coordinates for this area")
    let mapAreas = splitAreas coords garden 0 Map.empty
        maxAreaNb = Map.foldr max 0 mapAreas
    unless (maxAreaNb == 2) (error "Should have 3 separate regions")

    print "Tests passed!"
    -- Part 1
    let p1Test = part1 test_input
    unless (p1Test == 1930) (error $ "wrong result for example on part 1: " ++ (show p1Test))
    let p1 = part1 full_input
    printf "Part 1: %d\n" p1

    -- Part 2
    let p2Test = part2 test_input
    unless (p2Test == 1) (error $ "wrong result for example on part 2: " ++ (show p2Test))
    let p2 = part2 full_input :: Int
    printf "Part 2: %d\n" p2