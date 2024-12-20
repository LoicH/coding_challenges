module Main where

import Data.Char(digitToInt, intToDigit)
import qualified Data.Set as Set 
import qualified Data.List as List 
import qualified Data.Map as Map 
import Text.Printf(printf)
import Control.Monad(unless)
import Debug.Trace(trace, traceShow)

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

reverseMap :: Areas -> Map.Map Int [Coord] 
reverseMap areas = Map.foldrWithKey (\coord n m -> Map.insertWith (++) n [coord] m) Map.empty areas 

-- Returns the number of edges in a straight line 
countHorizontFences :: Areas -> Int -> Int -> Int -> Int -> Map.Map Int Int -> Map.Map Int Int 
countHorizontFences areas w i j prevArea edges = do
    if j == w 
    then Map.delete (-1) $ addFence prevArea edges 
    else countHorizontFences areas w i (j+1) k newMap 
    where Just k = Map.lookup (i,j) ({- traceShow ("i",i,"j",j) -} areas) 
          newMap = if prevArea==k then edges else addFence k (addFence prevArea edges)
          addFence k m = Map.insertWith (+) k 1 m

-- Returns the number of edges in a column 
countVerticalFences :: Areas -> Int -> Int -> Int -> Int -> Map.Map Int Int -> Map.Map Int Int 
countVerticalFences areas h i j prevArea edges = do
    if {- traceShow ("i",i,"j",j) -} i == h
    then Map.delete (-1) $ addFence prevArea edges 
    else countVerticalFences areas h (i+1) j k newMap 
    where Just k = Map.lookup (i,j) areas
          newMap = if prevArea==k then edges else addFence k (addFence prevArea edges)
          addFence k m = Map.insertWith (+) k 1 m



countAllFences :: Areas -> Int -> Int -> Map.Map Int Int 
countAllFences areas h w = do 
    let hors = List.foldl (\m i -> countHorizontFences areas w i 0 (-1) m) Map.empty [0..h-1]
        vfences = List.foldl (\m j -> countVerticalFences areas h 0 j (-1) m) Map.empty [0..w-1]
        fences = Map.unionWith (+) hors vfences
     in fences

countArea :: Areas -> Map.Map Int Int 
countArea areas = Map.fromListWith (+) [(n, 1) | n <- Map.elems areas]

computePrices :: Areas -> Int -> Int -> Int
computePrices areas h w = do
    let fences = countAllFences areas h w 
        sizes = countArea areas 
        sizesFences = zip (Map.toAscList sizes) (Map.toAscList fences)
        prices = [s*f | ((_, s), (_, f)) <- sizesFences]
     in sum prices


part1 :: String -> Int
part1 content = let garden = parse content 
                    h = length garden 
                    w = length $ garden!!0
                    coords = [(i,j) | i <- [0..h-1], j <- [0..w-1]]
                    areas = splitAreas coords garden 0 Map.empty
                 in computePrices areas h w 

-- Part 2

-- fences :: Eq a => [a] -> Map.Map a [Int]
-- fences [] = undefined
-- fences (x:xs) = 0 : (fences' xs 1 x)

-- fences' :: Eq a => [a] -> Int -> a -> Map.Map a [Int]
-- fences' [] i _ = [i]
-- fences' (x:xs) i prev = if x==prev
                        -- then fences' xs (i+1) prev 
                        -- else i : (fences' xs (i+1) prev)


-- Returns all the coordinates that separate areas in a column fashion
verticalFences :: Areas -> Int -> Int -> Map.Map Int [Coord]
verticalFences mapAreas h w = do 
    let vfences = List.foldl (\m j -> verticalFences' mapAreas h 0 j (-1) m) Map.empty [0..w-1]
     in vfences

verticalFences' mapAreas h i j prevArea fences = do 
    if i==h 
    then Map.delete (-1) $ addFence prevArea (2*i-1,j) fences
    else verticalFences' mapAreas h (i+1) j areaNb newMap 
    where Just areaNb = Map.lookup (i,j) mapAreas
          newMap = if prevArea==areaNb then fences else addFence areaNb (2*i,j) (addFence prevArea (2*i-1,j) fences)
          addFence n (i,j) m = Map.insertWith (++) n [(i,j)] m 


-- Returns all the coordinates that separate areas in a line
horizontalFences :: Areas -> Int -> Int -> Map.Map Int [Coord]
horizontalFences mapAreas h w = do 
    let hfences = List.foldl (\m i -> horizontalFences' mapAreas w i 0 (-1) m) Map.empty [0..h-1]
     in hfences

horizontalFences' mapAreas w i j prevArea fences = do 
    if j==w 
    then Map.delete (-1) $ addFence prevArea (i,2*j-1) fences
    else horizontalFences' mapAreas w i (j+1) areaNb newMap 
    where Just areaNb = Map.lookup (i,j) mapAreas
          newMap = if prevArea==areaNb then fences else addFence areaNb (i,2*j) (addFence prevArea (i,2*j-1) fences)
          addFence n (i,j) m = Map.insertWith (++) n [(i,j)] m 


-- This needs a sorted list, call this with (prevI,prevJ) = head of coords 
countSides' :: [Coord] -> Coord -> Int
countSides' [] _ = 1 
countSides' ((i,j):xs) (prevI, prevJ) = n + countSides' xs (i,j)
    where n = if i==prevI && j==prevJ+1
              then 0 
              else 1

countVertSides :: [Coord] -> Int 
countVertSides [] = 0
countVertSides coords = countSides' xs x
    where (x:xs) = List.sort coords

countHorSides :: [Coord] -> Int 
countHorSides coords = countSides' xs x 
    where (x:xs) = (List.sort [(j,i) | (i,j) <- coords])

countSides :: Areas -> Int -> Int -> Map.Map Int Int
countSides mapAreas h w = do 
    let vertFences = verticalFences mapAreas h w -- Map Int [Coord]
        vertSides = Map.map countVertSides vertFences
        horFences = horizontalFences mapAreas h w -- Map Int [Coord]
        horSides = Map.map countHorSides horFences
     in Map.unionWith (+) vertSides horSides

computePrice2 :: Areas -> Map.Map Int Int -> Int
computePrice2 mapAreas sidesCount = do 
    let sizes = countArea mapAreas 
        sizesSides = zip (Map.toAscList sizes) (Map.toAscList sidesCount)
        prices = [s*c | ((_, s), (_, c)) <- sizesSides]
     in sum prices

part2 :: String -> Int
part2 content = do
    let garden = parse content 
        h = length garden 
        w = length $ garden!!0
        coords = [(i,j) | i <- [0..h-1], j <- [0..w-1]]
        mapAreas = splitAreas coords garden 0 Map.empty
        sides = countSides mapAreas h w -- Map Int Int 
        price = computePrice2 mapAreas sides 
     in price

main :: IO ()
main = do
    putStrLn "Hello!"
    test_input <- readFile "test_input.txt"
    full_input <- readFile "input.txt"
    -- Tests 
    -- Small example
    let garden = parse "OOO\nXOX" 
        h = length garden 
        w = length $ garden!!0
        coords = [(i,j) | i <- [0..h-1], j <- [0..w-1]]
    unless (4 == length (dfs [(0,0)] garden (garden!!0!!0) 0 Map.empty)) (error $ "Should have 4 coordinates for this area")
    let mapAreas = splitAreas coords garden 0 Map.empty
        maxAreaNb = Map.foldr max 0 mapAreas
    unless (maxAreaNb == 2) (error "Should have 3 separate regions")
    print "fences"
    let vfences = verticalFences mapAreas h w 
    print vfences
    let hfences = horizontalFences mapAreas h w 
    print hfences 
    let sidesCount = countSides mapAreas h w 
    print sidesCount

    -- Test input
    let garden = parse test_input
        h = length garden 
        w = length $ garden!!0
        coords = [(i,j) | i <- [0..h-1], j <- [0..w-1]]
    unless (12 == length (dfs [(0,0)] garden (garden!!0!!0) 0 Map.empty)) (error $ "'R' area should have an area of 12")
    let mapAreas = splitAreas coords garden 0 Map.empty
        maxAreaNb = Map.foldr max 0 mapAreas
    unless (maxAreaNb == 10) (error "Should have 11 separate regions")
    print mapAreas

    -- Other tests for P2
    let testABCDE = "AAAA\nBBCD\nBBCC\nEEEC"
        res = part2 testABCDE
    unless (80 == res) (error $ "Wrong answer for example ABCDE:"++ (show res))

    print "example OX"
    let testOX = "OOOOO\nOXOXO\nOOOOO\nOXOXO\nOOOOO"
        res = part2 testOX
    unless (436 == res) (error $ "Wrong answer for example OX:"++ (show res))

    print "example EX"
    let testEX = "EEEEE\nEXXXX\nEEEEE\nEXXXX\nEEEEE"
        res = part2 testEX
    unless (236 == res) (error $ "Wrong answer for example EX:"++ (show res))

    print "example AB"
    let testAB = "AAAAAA\nAAABBA\nAAABBA\nABBAAA\nABBAAA\nAAAAAA\n"
        res = part2 testAB
    unless (368 == res) (error $ "Wrong answer for example AB:"++ (show res))
    -- Full input 
    let garden = parse full_input 
        h = length garden 
        w = length $ garden!!0
        coords = [(i,j) | i <- [0..h-1], j <- [0..w-1]]
        mapAreas = splitAreas coords garden 0 Map.empty
        sides = countSides mapAreas h w -- Map Int Int 
        price = computePrice2 mapAreas sides 
    print ("sides"++show sides)


    print "Tests passed!"

    -- Part 1
    let p1Test = part1 test_input
    unless (p1Test == 1930) (error $ "wrong result for example on part 1: " ++ (show p1Test))
    let p1 = part1 full_input
    printf "Part 1: %d\n" p1

    -- Part 2
    let p2Test = part2 test_input
    unless (p2Test == 1206) (error $ "wrong result for example on part 2: " ++ (show p2Test))
    let p2 = part2 full_input :: Int
    printf "Part 2: %d\n" p2
