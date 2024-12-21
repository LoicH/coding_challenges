module Main where

import Data.Char(digitToInt, intToDigit)
import qualified Data.Set as Set 
import qualified Data.List as List 
import qualified Data.Map as Map 
import Text.Printf(printf)
import Control.Monad(unless)
import Debug.Trace(trace,traceShow)
import Control.Concurrent(threadDelay)

type Coord = (Int, Int)
type Robot = (Coord, Coord) -- Position, speed

parseLine :: String -> Robot 
parseLine ('p':'=':l) = do -- transform "p=0,4 v=3,-3" into ((0,4), (3,-3))
    let (j, ',':r1) = (span ((/=) ',') l)        -- r1 = "4 v=3,-3"
        (i, ' ':'v':'=':r2) = span ((/=) ' ') r1 -- r2 == "3,-3"
        (dj, ',':di) = span ((/=) ',') r2
     in ((read i, read j), (read di, read dj))

parse :: String -> [Robot]
parse c = map parseLine $ lines c 

step :: Int -> Int -> Robot -> Robot 
step h w ((i,j), (di,dj)) = (((i+di) `mod` h, (j+dj) `mod` w), (di, dj))

steps :: Int -> Int -> Int -> Robot -> Robot 
steps h w n robot = List.foldl (\r _ -> step h w r) robot [1..n]

partition' :: Int -> Int -> (Int,Int,Int,Int) -> Robot -> (Int,Int,Int,Int)
partition' h w (ul,ur,bl,br) ((i,j), _)
    | i < (h-1) `div` 2 && j < (w-1) `div` 2 = (ul+1, ur,bl,br)
    | i < (h-1) `div` 2 && j > (w-1) `div` 2 = (ul,ur+1, bl,br)
    | i > (h-1) `div` 2 && j < (w-1) `div` 2 = (ul,ur,bl+1, br)
    | i > (h-1) `div` 2 && j > (w-1) `div` 2 = (ul,ur,bl, br+1)
    | otherwise = (ul,ur,bl,br)

countQuadrants :: Int -> Int -> [Robot] -> (Int,Int,Int,Int)
countQuadrants h w robots = List.foldl (partition' h w) (0,0,0,0) robots

robotsToLine :: Int -> Int -> Map.Map Coord Coord -> String 
robotsToLine i w robots = List.foldl (\s j -> if (i,j) `Map.member` robots then s++"x" else s++".") "" [0..w-1]

robotsToString :: Int -> Int -> Map.Map Coord Coord -> String 
robotsToString h w robots = List.foldl (\s i -> s ++ "\n" ++ (robotsToLine i w robots)) "" [0..h-1]

part1 :: String -> Int -> Int -> Int
part1 content h w = do
    let robots = parse content 
        endRobots = map (steps h w 100) robots 
        (a,b,c,d) = traceShow endRobots $ countQuadrants h w endRobots

     in traceShow (a,b,c,d) a*b*c*d

minArea :: [Robot] -> Int
minArea robots = 
    let is = [i | ((i,_), _) <- robots ]
        js = [j | ((_,j), _) <- robots ]
        di = List.maximum is - List.minimum is 
        dj = List.maximum js - List.minimum js 
      in di*dj

count :: Int -> [Int] -> [Float]
count maxx xs = do
    let start = Map.fromList (List.zip [0..maxx] (repeat 0))
        counts = List.foldl (\m n -> Map.insertWith (+) n 1 m) start xs 
     in  Map.elems counts 

statj :: Int -> [Robot] -> Float
statj w robots = std (count w [j | ((_,j), _) <- robots])

part2 :: String -> Int -> Int -> ([[Robot]], [Float])
part2 content h w = do
    let robots = parse content 
        allRobots = List.scanl (\robots n -> map (step h w) robots) robots [1..]
        stdDevs = map (statj w) allRobots
        -- areas = map minArea allRobots
     in (allRobots, stdDevs)

frame :: Int -> Int -> [Robot] -> IO ()
frame h w robots = do
    let str = robotsToString h w . Map.fromList $ robots
    putStrLn str
    threadDelay 161700

avg :: Fractional b => [b] -> b 
avg = (/) <$> sum <*> realToFrac . length 

std :: [Float] -> Float 
std xs = sqrt . avg . map ((^2) . (-) axs) $ xs 
    where axs = avg xs 

main :: IO ()
main = do
    putStrLn "Hello!"
    test_input <- readFile "test_input.txt"
    full_input <- readFile "input.txt"
    -- Tests 
    let robots = parse test_input
        (h,w) = (7,11)
    unless (12 == length robots) (error "Should have 12 robots")

    let endRobots = map (steps h w 100) robots 
        mapRobots = Map.fromList endRobots

    putStrLn $ robotsToString h w mapRobots
    print "Tests passed!"

    -- Part 1
    let p1Test = part1 test_input 7 11 
    unless (p1Test == 12) (error $ "wrong result for example on part 1: " ++ (show p1Test))
    let p1 = part1 full_input 103 101
    printf "Part 1: %d\n" p1

    -- Part 2
    -- let p2Test = part2 test_input
    -- unless (p2Test == 1) (error $ "wrong result for example on part 2: " ++ (show p2Test))
    -- let p2 = part2 full_input 
    let (h,w) = (103,101)
        (allRobots, stdDevs) = part2 full_input h w 
    -- print $ List.scanl (\prevMin area -> if area <= prevMin then area else prevMin) (103*101) areas

    -- print (List.scanl (\(iMax,stdMax) (s, i) -> if s > stdMax then (i,s) else (iMax, stdMax)) (0,0) (zip stdDevs [1..]))
    mapM_ (frame h w) allRobots

