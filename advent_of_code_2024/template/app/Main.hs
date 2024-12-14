module Main where

import Data.Char(digitToInt, intToDigit)
import qualified Data.Set as Set 
import Text.Printf(printf)
import Control.Monad(unless)
import Debug.Trace(trace)

parse :: String -> () 
parse content = return ()

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
    let p2 = part2 full_input :: Int
    printf "Part 2: %d\n" p2