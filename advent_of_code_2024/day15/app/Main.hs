module Main where

import Data.Char(digitToInt, intToDigit)
import qualified Data.Set as Set 
import qualified Data.List as List 
import qualified Data.Map as Map
import Data.Map((!),(!?)) 
import Text.Printf(printf)
-- import Text.Read(read)
import Control.Monad(unless)
import Debug.Trace(trace,traceShow)
import Control.Concurrent(threadDelay)
import qualified Data.Text as T

type Coord = (Int, Int)
type Plan = Map.Map Coord Object
data Object = Wall | Box  deriving (Eq, Show)


charToObj :: Char -> Object
charToObj c = case c of 
    '#' -> Wall
    'O' -> Box 
    otherwise -> undefined

data Wharehouse = Wharehouse {plan :: Plan
                            , robot :: Coord }  deriving (Eq, Show)

parseLine :: String -> Int -> Plan 
parseLine l i = let objects = filter (\(c,j) -> (c=='O' || c=='#')) (zip l [0..])
                    assocList = [((i,j), charToObj obj) | (obj,j) <- objects] :: [(Coord, Object)]
                 in Map.fromList assocList


-- Returns the wharehouse and the moves
parse :: String -> (Wharehouse, String)
parse c = let (plan, moves) = List.span ((/=) mempty) $ lines c 
              allMoves = List.foldl (++) "" moves
             -- Let's find the robot's coordinates
              (lineRobot, iRobot) = head $ filter (\(l,_) -> '@' `elem` l) (zip plan [0..]) 
              jRobot = snd . head . filter (((==) '@') . fst) $ zip lineRobot $ [0..]
              -- Build the floor plan of the wharehouse
              linesPlan = List.map (\(l,i) -> parseLine l i) (zip plan [0..])
              floorPlan = List.foldl Map.union Map.empty linesPlan
            in (Wharehouse {plan=floorPlan, robot=(iRobot, jRobot)}, allMoves)


getDir :: Char -> (Int, Int)
getDir c = case c of 
    '^' -> (-1,0)
    'v' -> (1,0)
    '>' -> (0,1)
    '<' -> (0,-1)

countBoxes :: Plan -> Coord -> Coord -> Int 
countBoxes plan (i,j) (di,dj) = -- (i,j) is the position of the robot, (i+di, j+dj) is the 1st box
    let boxes = List.takeWhile (\k -> Map.lookup (i+k*di, j+k*dj) plan == Just Box) [1..]
        --boxes = [(i+k*di, j+k*dj) | k <- [1..], Map.lookup (i+k*di, j+k*dj) plan == Just Box] -- traceShow (i+k*di, j+k*dj)
     in length boxes

pushBoxes :: Wharehouse -> Coord -> Coord -> Wharehouse
pushBoxes wh firstBox freeSpace = 
    let p = plan wh
        newPlan = Map.insert freeSpace Box (Map.delete firstBox p)
     in Wharehouse {plan=newPlan, robot=firstBox}

move :: Wharehouse -> Char -> Wharehouse
move wh m = do 
    let (di,dj) = {- traceShow (m, getDir m) -} (getDir m)
        (ri,rj) = robot wh 
        p = plan wh 
     in case Map.lookup (ri+di, rj+dj) p of 
        Nothing -> {- trace "Moving" -} Wharehouse {plan=p, robot=(ri+di,rj+dj)}
        Just Wall -> {- trace "Wall" -} wh
        Just Box -> let nb = {- trace "Counting boxes" -} countBoxes p (ri,rj) (di, dj)
                        nextPos = (ri+(nb+1)*di, rj+(nb+1)*dj)
                        nextObj = Map.lookup nextPos p 
                     in case nextObj of 
                        Just Wall -> wh 
                        Just Box -> undefined
                        Nothing -> pushBoxes wh (ri+di, rj+dj) nextPos

result :: Coord -> Int 
result (i,j) = 100*i+j

computeResult :: Plan -> Int 
computeResult p = do 
    let boxes = Map.filter ((==) Box) p 
        results = Map.foldrWithKey (\(i,j) _ accu -> result (i,j) + accu) 0 boxes 
     in results

planToChar :: Plan -> Coord -> Coord -> Char
planToChar p c r = if c == r then '@' else case Map.lookup c p of 
    Nothing -> '.'
    Just Wall -> '#'
    Just Box -> 'O'

-- planToString :: Plan -> String 
wharehouseToString Wharehouse{plan=p, robot=(ri,rj)} = do 
    let ((h,w), _) = Map.findMax p 
        ls = [[ planToChar p (i,j) (ri,rj) | j <- [0..w]] | i <- [0..h]]
     in List.foldl (\accu l -> accu++l++"\n") "" ls 

part1 :: String -> Int
part1 content = do 
    let (wh,moves) = parse content 
        endWh = List.foldl move wh moves 
     in computeResult (plan endWh)

-- Part 2 Logic
enlarge :: Wharehouse -> Wharehouse
enlarge wh = let ((ri,rj), p) = (robot wh, plan wh)
                 newR = (ri, 2*rj)
                 newPlan = Map.mapKeysMonotonic (\(i,j) -> (i,2*j)) p 
                 walls = Map.filter ((==) Wall) newPlan 
                 newWalls = Map.mapKeysMonotonic (\(i,j) -> (i,j+1)) walls 
             in Wharehouse {plan=Map.union newPlan newWalls, robot=newR}

parse2 :: String -> (Wharehouse, String)
parse2 c = let (wh, moves) = parse c 
            in (enlarge wh, moves)

updateKey :: Ord k => k -> k -> Map.Map k a -> Map.Map k a                 
updateKey oldK newK m = let v = m!oldK 
                         in Map.insert newK v (Map.delete oldK m)

planToChar2 :: Plan -> Coord -> Coord -> Char
planToChar2 p c r = if c == r then '@' else case Map.lookup c p of 
    Nothing -> '.'
    Just Wall -> '#'
    Just Box -> '['
    
wharehouseToString2 Wharehouse{plan=p, robot=(ri,rj)} = do 
    let ((h,w), _) = Map.findMax p 
        ls = [[ planToChar2 p (i,j) (ri,rj) | j <- [0..w]] | i <- [0..h]]
        fullStr = List.foldl (\accu l -> accu++l++"\n") "" ls 
     in T.unpack $ T.replace (T.pack "[.") (T.pack "[]") (T.pack fullStr)

move2 :: Wharehouse -> Char -> Wharehouse
move2 wh m = do 
    let (di,dj) = {- traceShow (m, getDir m) -} (getDir m)
        (ri,rj) = robot wh 
        p = plan wh 
     in case traceShow (di,dj, p!?(ri+di, rj+dj-1), p!?(ri+di, rj+dj)) (di,dj, p!?(ri+di, rj+dj-1), p!?(ri+di, rj+dj)) of 
        (_,_,_,Just Wall) -> wh 
        (0,1,_,Nothing) -> Wharehouse {plan=p, robot=(ri, rj+1)} -- Moving to the right
        (0,1,_,Just Box) -> let boxes = List.takeWhile (\k -> p!?(ri, rj+2*k+1)==Just Box) [0..] -- Push to the right
                                nboxes = length boxes 
                                nextPos = (ri, rj+2*nboxes+1)
                                in case p!?nextPos of 
                                    Just Wall -> wh 
                                    Nothing -> let newPlan = List.foldl (\p k -> updateKey (ri, rj+2*k+1) (ri, rj+2*(k+1)) p) p boxes 
                                                   newRobot = (ri, rj+1)
                                                 in Wharehouse {plan=newPlan, robot=newRobot}
        (0,-1,Nothing,Nothing) -> Wharehouse {plan=p, robot=(ri, rj-1)} -- Moving to left, no box on our left
        (0,-1,Just Wall,Nothing) -> Wharehouse {plan=p, robot=(ri, rj-1)} -- Moving to left, no box on our left
        (0,-1,_,Just Box) -> undefined -- We can't be on the right half of a box
        (0,-1,Just Box,Nothing) -> let boxes = List.takeWhile (\k -> p!?(ri, rj-2*(k+1))==Just Box) [0..] -- There is a box on our left 
                                       nboxes = length boxes 
                                       nextPos = (ri, rj-2*nboxes-1)
                                     in case p!?nextPos of 
                                        Just Wall -> wh 
                                        Nothing -> let newPlan = List.foldl (\p k -> updateKey (ri, rj-2*(k+1)) (ri, rj-2*(k+1)-1) p) p boxes 
                                                       newRobot = (ri, rj-1)
                                                     in Wharehouse {plan=newPlan, robot=newRobot}
        (_,0,Just Box, Just Box) -> undefined -- Can't have two large boxes sitting so close
        (_,0,_, Just Box) -> case vpush (ri+di,rj) di p of 
                                   Nothing -> wh -- We couldn't move anything
                                   Just newPlan -> Wharehouse {plan=newPlan, robot=(ri+di, rj)}
        (_,0,Just Box,Nothing) -> case vpush (ri+di,rj-1) di p of 
                                   Nothing -> wh -- We couldn't move anything
                                   Just newPlan -> Wharehouse {plan=newPlan, robot=(ri+di, rj)}
        (_,0,_,Nothing) -> Wharehouse {plan=p, robot=(ri+di, rj)} -- Moving vertically without boxes in our path


vpush :: Coord -> Int -> Plan -> Maybe Plan 
vpush b@(i,j) di p = case traceShow (p!?(i+di,j-1),p!?(i+di,j),p!?(i+di,j+1)) (p!?(i+di,j-1),p!?(i+di,j),p!?(i+di,j+1)) of -- Checking the 3 cells above/below the box being pushed
    (Nothing, Nothing, Nothing) -> Just (updateKey b (i+di,j) p)
    (Just Wall, Nothing, Nothing) -> Just (updateKey b (i+di,j) p)
    (_, Just Box, Nothing) -> case vpush (i+di,j) di p of -- Just another box above/below the one being pushed
                                Nothing -> Nothing 
                                Just newPlan -> Just (updateKey b (i+di,j) newPlan)
    (_, Just Box, _) -> undefined -- There can't be a box and something directly on the right
    (_, Just Wall,_) -> Nothing   -- A wall in front on the left part of the box
    (_, _, Just Wall) -> Nothing  -- A wall in front on the right part of the box
    (Just Box, _, Just Box) ->  case vpush (i+di, j-1) di p of  -- Two boxes in front of us
                                Nothing -> Nothing 
                                Just newPlan -> case vpush (i+di, j+1) di newPlan of 
                                    Nothing -> Nothing 
                                    Just newNewPlan -> Just (updateKey b (i+di,j) newNewPlan)
    (Just Box, _, _) -> case vpush (i+di, j-1) di p of  -- A box in the left diagonal
                        Nothing -> Nothing 
                        Just newPlan ->  Just (updateKey b (i+di,j) newPlan)
    (_, _, Just Box) -> case vpush (i+di, j+1) di p of  -- A box in the right diagonal
                        Nothing -> Nothing 
                        Just newPlan ->  Just (updateKey b (i+di,j) newPlan)


part2 :: String -> Int
part2 content = do
    let (wh,moves) = parse2 content 
        endWh = List.foldl move2 wh moves 
     in computeResult (plan endWh)


main :: IO ()
main = do
    putStrLn "Hello!"
    small_test_input <- readFile "small_test_input.txt"
    larger_test_input <- readFile "large_test_input.txt"
    full_input <- readFile "input.txt"
    -- Tests 
    -- Small example 
    -- 30 walls, 6 boxes
    let (wh, moves) = parse2 small_test_input
    print . robot $ wh
    putStrLn . wharehouseToString2 $ wh 
    unless (robot wh == (2,4)) (error "Robot should start at (2,4)")
    unless (Map.size (plan wh) == 66) (error "Small test example should have 66 objects")
    
    let (wh, moves) = parse2 larger_test_input
    putStrLn . wharehouseToString2 $ wh 
    putStrLn . wharehouseToString2 $ (move2 wh (moves!!0))
    -- let p1Test = part1 larger_test_input
    -- unless (p1Test == 10092) (error $ "wrong result for larger example on part 1: " ++ (show p1Test))
    print "Tests passed!"

    -- Part 1
    let p1Test = part1 small_test_input
    unless (p1Test == 2028) (error $ "wrong result for smaller example on part 1: " ++ (show p1Test))
    let p1 = part1 full_input
    printf "Part 1: %d\n" p1

    -- Part 2
    let p2Test = part2 larger_test_input
    unless (p2Test == 9021) (error $ "wrong result for example on part 2: " ++ (show p2Test))
    let p2 = part2 full_input 
    printf "Part 2: %d\n" p2
