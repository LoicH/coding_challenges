module Main where

import Data.List as List
import Data.Map as Map 
import Data.Set as Set 
import Text.Printf(printf)
import Control.Monad 
import Debug.Trace


type Equation = (Int, [Int])

parse :: String -> [Equation]
parse content = List.map parseLine $ lines content

parseLine :: String -> Equation
parseLine l =
    let (testVal, _:_:remain) = span ((/=) ':') l 
    in (read testVal, List.map read (words remain))

part1 :: String -> Int
part1 content = 
    let eqs = parse content
        validEqs = List.filter checkEquation eqs
    in List.sum (List.map fst validEqs)

checkEquation :: Equation -> Bool
checkEquation (testVal, xs) = testVal `elem` (results . reverse $ xs)

results :: [Int] -> Set Int
results [x] = Set.singleton x
results (x:xs) = Set.union (Set.map (+x) res)  (Set.map (*x) res)
    where res = results xs


-- Part 2
concatReverse :: Int -> Int -> Int 
concatReverse a b = read (show b ++ show a)

part2 :: String -> Int
part2 content = 
    let eqs = parse content
        validEqs = List.filter checkEquation2 eqs
    in List.sum (List.map fst validEqs)

checkEquation2 :: Equation -> Bool
checkEquation2 (testVal, xs) = testVal `elem` (results2 . reverse $ xs)

results2 :: [Int] -> Set Int
results2 [x] = Set.singleton x
results2 (x:xs) = Set.union (Set.union (Set.map (+x) res)  (Set.map (*x) res)) (Set.map (concatReverse x) res)
    where res = results2 xs


main :: IO ()
main = do
    test_input <- readFile "test_input.txt"
    -- Part 1
    let p1Test = part1 test_input
    unless (p1Test == 3749) (error $ "wrong result for example on part 1: " ++ (show p1Test))
    full_input <- readFile "input.txt"
    let p1 = part1 full_input
    printf "Part 1: %d\n" p1

    -- Part 2
    let p2Test = part2 test_input
    unless (p2Test == 11387) (error $ "wrong result for example on part 2: " ++ (show p2Test))
    let p2 = part2 full_input
    printf "Part 2: %d\n" p2
