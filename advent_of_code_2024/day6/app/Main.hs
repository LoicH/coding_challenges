module Main where

import Data.List as List
import Data.Map as Map 
import Data.Set as Set 
import Text.Printf(printf)
import Control.Monad 
import Debug.Trace

directions = [(-1, 0), (0,1), (1, 0), (0, -1)]
nextDir :: (Int, Int) -> (Int, Int)
nextDir (a,b) = (b, -a)


-- return the positions of all obstacles and the starting position
parse :: String -> ([(Int, Int)], (Int, Int), Int, Int)
parse content = 
    let ls = lines content
        n = length ls
        w = length (ls !! 0)
        obstacles = [(i, findIndices ((==) '#') l) | (i, l) <- zip [0..] $ ls] -- list of (Int, [Int])
        obstCoord = obstacles >>= (\(a,b) -> zip (repeat a) b)
        Just i = List.findIndex (elem '^') (ls)
        Just j = List.findIndex ((==) '^') (ls !! i)
    in (obstCoord, (i,j), n, w)

walk :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> Int -> Int -> Set (Int, Int) -> Set (Int, Int)
walk start@(i,j) dirs obstacles height width positions = 
    let (di, dj) = head dirs 
        (newI, newJ) = (i+di, j+dj)
    in if newI < 0 || newJ < 0 || newI >= height || newJ >= width
       then (Set.insert start positions)
       else if (newI, newJ) `elem` obstacles
       then walk start (tail dirs) obstacles height width positions
       else walk (newI, newJ) dirs obstacles height width (Set.insert start positions)

part1 :: String -> Int
part1 content = do 
    let (obstCoord, (iStart, jStart), h, w) = parse content
        visited = walk (iStart, jStart) (cycle [(-1, 0), (0,1), (1, 0), (0, -1)]) obstCoord h w (Set.singleton (iStart, jStart))
    length visited


createLoop :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> Int -> Int -> Set ((Int, Int), (Int, Int)) -> Map.Map ((Int, Int),(Int, Int)) (Maybe ((Int, Int),(Int, Int))) -> Bool
createLoop start@(i,j) dir@(di,dj) obstacles newObs@(iObs, jObs) height width history jumpMap = 
-- return True is this configuration results in the guardian ends up walking in a loop
    let arrival = {-trace ("start= " ++ show start ++ "\tdir= " ++ show dir) -}(Map.lookup (start, dir) jumpMap)
    in case arrival of 
        Nothing -> error ("We did not compute the jump from " ++ show (start, dir)) -- we exit the map
        -- we should get off the map, but is there a new obstacle?
        Just Nothing -> {-trace ("We get off the map, from start= " ++ show start) -}(
                let steps = [(i+(di*k), j+(dj*k)) | k <- [0..max height width]]
                    valids = takeWhile (walkable obstacles height width) steps
                in if newObs `elem` valids
                   then let nextPos = {-trace ("Should get off map but met "++ show newObs)-} (iObs-di, jObs-dj)
                        in createLoop nextPos (nextDir dir) obstacles newObs height width (Set.insert (start, dir) history) jumpMap
                   else False)
        Just (Just ((iNew, jNew), newDir)) -> -- We need to check if the new obstacle is between start and newPos
            -- If it is: we keep the newDir, but the newPos is one step before encountering this obstacle
            -- If it's not, we keep newPos and newDir
            -- add current position and direction to history, and go to the next step
            let nextPos = if (iNew == iObs && jObs >= min j jNew && jObs <= max j jNew) || (jNew == jObs && iObs >= min i iNew && iObs <= max i iNew)
                        then (iObs-di, jObs-dj)
                        else (iNew, jNew)
                -- debug = trace ("new= "++  show (iNew,jNew) ++ "\tobs= "++ show newObs++"\tnextPost= "++ show nextPos) True
            in if (nextPos, nextDir dir) `elem` history
               then True 
               else createLoop nextPos newDir obstacles newObs height width (Set.insert (start, dir) history) jumpMap

part2 :: String -> Int
part2 content = 
    let (obstCoord, start, h, w) = parse content
        newObs = newObstacles obstCoord start h w
        dir = head directions
        jumpMap = getJumpMap obstCoord h w
        loops = List.filter id [createLoop start dir obstCoord obs h w Set.empty jumpMap | obs <- newObs]
    in length loops 

newObstacles :: [(Int, Int)] -> (Int, Int) -> Int -> Int -> [(Int, Int)]
newObstacles obstacles start h w = 
    let visited = walk start (cycle [(-1, 0), (0,1), (1, 0), (0, -1)]) obstacles h w (Set.singleton start)
    in Set.toList (visited Set.\\ (Set.singleton start))

outOfMap :: Int -> Int -> (Int, Int) -> Bool
outOfMap h w (i,j) = i < 0 || j < 0 || i >= h || j >= w

-- Don't use this! We can walk on the border
-- onTheBorder h w (i,j) = i*j == 0 || i==h || j==w 

-- beware of a corner with two obstacles!
walkable :: [(Int, Int)] -> Int -> Int -> (Int, Int) -> Bool
walkable obstacles h w (i,j) = not (outOfMap h w (i,j)) && not ((i,j) `elem` obstacles)

-- return Maybe ((destI, destJ), (newDI, newDJ))
-- if we get out of the map from this jump, return Nothing
-- else, return where we stopped, and our next direction
getJumpFinish :: [(Int, Int)] -> Int -> Int -> (Int, Int) -> (Int, Int) -> Maybe ((Int, Int),(Int, Int))
getJumpFinish obstacles h w start@(i,j) (di, dj) = 
    if start `elem` obstacles then Nothing
    else 
        let line = [(i+(di*k), j+(dj*k)) | k <- [0..(max h w)]]
            (valids, invalids) = span (walkable obstacles h w) line
        -- we need to compute all the steps we can do until we fall off the map
        -- or we encounter an obstacle.
        -- if the last step is an obstacle, return the last valid position, with a new direction
        -- else, we fell off the map, and we return "Nothing"
            firstIllegalStep = head invalids 
        in if outOfMap h w firstIllegalStep 
        then Nothing 
        else Just (head . reverse $ valids, nextDir (di, dj))
    

-- return a map of ((i,j), (di,dj)) -> Maybe ((i,j), (di,dj))
-- If we get Nothing, this means that we jump out of the map
getJumpMap :: [(Int, Int)] -> Int -> Int -> Map.Map ((Int, Int),(Int, Int)) (Maybe ((Int, Int),(Int, Int)))
getJumpMap obstacles h w = 
    let starts = [((i, j), (di, dj)) | i <- [0..h-1], j <- [0..w-1], (di,dj) <- directions]
        jumps = [((start, dir), getJumpFinish obstacles h w start dir) | (start, dir) <- starts]
    in Map.fromList jumps


main :: IO ()
main = do
    test_input <- readFile "test_input.txt"
    let p1Test = part1 test_input
    unless (p1Test == 41) (error $ "wrong result for example on part 1: " ++ (show p1Test))
    full_input <- readFile "input.txt"
    let p1 = part1 full_input
    printf "Part 1: %d\n" p1

    let p2Test = part2 test_input
        (obstCoord, start, h, w) = parse test_input
    
    print (getJumpMap obstCoord h w)
    let newObs = newObstacles obstCoord start h w 
        dir = head directions
        jumpMap = getJumpMap obstCoord h w
        tries = [createLoop start dir obstCoord obs h w Set.empty jumpMap | obs <- newObs]

    print (createLoop start dir obstCoord (6,3) h w Set.empty jumpMap)
    print (createLoop start dir obstCoord (7,6) h w Set.empty jumpMap)
    print (createLoop start dir obstCoord (7,7) h w Set.empty jumpMap)
    print (createLoop start dir obstCoord (8,1) h w Set.empty jumpMap)
    print (createLoop start dir obstCoord (8,3) h w Set.empty jumpMap)
    print ((trace "before 9 7") (createLoop start dir obstCoord (9,7) h w Set.empty jumpMap))
    -- print newObs
    -- print tries
    print "9 7 done"
    unless (p2Test == 6) (error $ "wrong result for example on part 2: " ++ (show p2Test))
    -- -- let (obstCoord, start, h, w) = parse full_input
    -- --     newObs = newObstacles obstCoord start h w
    -- -- print newObs
    -- -- let dirs = cycle [(-1, 0), (0,1), (1, 0), (0, -1)]
    -- --     tries = [createLoop start dirs (obs:obstCoord) h w Set.empty | obs <- [newObs]]
    -- -- print tries 
    
    let p2 = part2 full_input
    printf "Part 2: %d\n" p2

