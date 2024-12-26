module Main where

import Data.Char(digitToInt, intToDigit)
import qualified Data.Set as Set 
import qualified Data.List as List 
import qualified Data.Map as Map 
import Text.Printf(printf)
-- import Text.Read(read)
import Control.Monad(unless)
import Debug.Trace(trace,traceShow)
import Control.Concurrent(threadDelay)

type Coord = (Int, Int)
type BigPlan = Map.Map Coord BigObject

data Object = Wall | Box  deriving (Eq, Show)

data BigObject = BWall | LBox | RBox deriving (Eq, Show)

toBigObjLeft :: (Coord, Object) -> (Coord, BigObject)
toBigObjLeft ((i,j), o) = ((2*i, j), if o == Wall then BWall else LBox)

toBigObjRight :: (Coord, Object) -> (Coord, BigObject)
toBigObjRight ((i,j), o) = ((2*i+1, j), if o == Wall then BWall else RBox)

planToBigObj :: Map.Map Coord Object -> BigPlan
planToBigObj mObj = do 
    let assoc = Map.toList mObj
        newAssoc = [toBigObjLeft, toBigObjRight] <*> assoc 
     in Map.fromList newAssoc

charToObj :: Char -> Object
charToObj c = case c of 
    '#' -> Wall
    'O' -> Box 
    otherwise -> undefined

data Wharehouse = Wharehouse {plan :: Map.Map Coord Object
                            , robot :: Coord }  deriving (Eq, Show)

data BigWharehouse = BigWharehouse {bigPlan :: BigPlan 
                                    , bigRobot :: Coord} deriving (Eq, Show)

toBigWharehouse :: Wharehouse -> BigWharehouse
toBigWharehouse wh = let (ri,rj) = robot wh
                         p = plan wh 
                         in BigWharehouse {bigPlan=planToBigObj p, bigRobot=(2*ri, rj)}

parseLine :: String -> Int -> Map.Map Coord Object 
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

parse2 :: String -> (BigWharehouse, String)
parse2 c = let (wh, moves) = parse c 
            in (toBigWharehouse wh, moves)
                

getDir :: Char -> (Int, Int)
getDir c = case c of 
    '^' -> (-1,0)
    'v' -> (1,0)
    '>' -> (0,1)
    '<' -> (0,-1)

countBoxes :: Map.Map Coord Object -> Coord -> Coord -> Int 
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
    let (di,dj) = traceShow (m, getDir m) (getDir m)
        (ri,rj) = robot wh 
        p = plan wh 
     in case Map.lookup (ri+di, rj+dj) p of 
        Nothing -> trace "Moving" Wharehouse {plan=p, robot=(ri+di,rj+dj)}
        Just Wall -> trace "Wall" wh
        Just Box -> let nb = trace "Counting boxes" countBoxes p (ri,rj) (di, dj)
                        nextPos = (ri+(nb+1)*di, rj+(nb+1)*dj)
                        nextObj = Map.lookup nextPos p 
                     in case nextObj of 
                        Just Wall -> wh 
                        Just Box -> undefined
                        Nothing -> pushBoxes wh (ri+di, rj+dj) nextPos

result :: Coord -> Int 
result (i,j) = 100*i+j

computeResult :: Map.Map Coord Object -> Int 
computeResult p = do 
    let boxes = Map.filter ((==) Box) p 
        results = Map.foldrWithKey (\(i,j) _ accu -> result (i,j) + accu) 0 boxes 
     in results

planToChar :: Map.Map Coord Object -> Coord -> Coord -> Char
planToChar p c r = if c == r then '@' else case Map.lookup c p of 
    Nothing -> '.'
    Just Wall -> '#'
    Just Box -> 'O'

-- planToString :: Map.Map Coord Object -> String 
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
updateKey oldK newK m = let v = m!oldK 
                         in Map.insert newK v (Map.delete oldK m)

auxPush :: Coord -> Coord -> BigPlan -> Maybe BigPlan
auxPush c@(i,j) dir@(di,dj) p = let nextPos = (i+di,j+dj) 
                                 in case p!?nextPos of
    Nothing -> Just (updateKey c nextPos p)
    Just BWall -> Nothing 
    _ -> case push nextPos dir p of 
        Nothing -> Nothing 
        Maybe newPlan -> Just (updateKey c nextPos newPlan)

-- push :: Coord -> Coord -> BigPlan -> Maybe BigPlan
-- push c@(i,j) dir@(di,dj) p = let nextPos = (i+di,j+dj) 
--                               in case

-- je pense que ça marche si on pousse vers le haut ou le bas, mais horizontalement ??


part2 :: String -> Int
part2 content = 0 

main :: IO ()
main = do
    putStrLn "Hello!"
    small_test_input <- readFile "small_test_input.txt"
    larger_test_input <- readFile "large_test_input.txt"
    full_input <- readFile "input.txt"
    -- Tests 
    -- Small example 
    -- 30 walls, 6 boxes
    let (wh, moves) = parse small_test_input
    unless (robot wh == (2,2)) (error "Robot should start at (2,2)")
    unless (Map.size (plan wh) == 36) (error "Small test example should have 36 objects")

    let (wh, moves) = parse larger_test_input
    putStrLn . wharehouseToString $ wh 
    putStrLn . wharehouseToString $ (move wh (moves!!0))
    -- let p1Test = part1 larger_test_input
    -- unless (p1Test == 10092) (error $ "wrong result for larger example on part 1: " ++ (show p1Test))
    print "Tests passed!"

    -- Part 1
    let p1Test = part1 small_test_input
    unless (p1Test == 2028) (error $ "wrong result for smaller example on part 1: " ++ (show p1Test))
    let p1 = part1 full_input
    printf "Part 1: %d\n" p1

    -- Part 2
    let p2Test = part2 small_test_input
    unless (p2Test == 1) (error $ "wrong result for example on part 2: " ++ (show p2Test))
    let p2 = part2 full_input 
    printf "Part 2: %d\n" p2
