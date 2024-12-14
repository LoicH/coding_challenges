module Main where

import Data.Char(digitToInt, intToDigit)
import qualified Data.Set as Set 
import qualified Data.List as List 
import qualified Data.Map as Map 
import Text.Printf(printf)
import Control.Monad(unless)
import Debug.Trace(trace)

parse :: String -> [Int]
parse content = map read (words content)

digitsNumber :: Int -> Int 
digitsNumber = length . show 

splitNumber :: Int -> [Int]
splitNumber n = let l = (digitsNumber n ) `div` 2
                    (right, left) = n `quotRem` (10^l)
                 in [right, left]

jump :: Int -> [Int]
jump 0 = [1]
jump n = if digitsNumber n `mod` 2 == 0
         then splitNumber n 
         else [2024*n]

flatten :: [[a]] -> [a]
flatten xs = List.foldl (++) [] xs

-- repeat a function 5 times
repeat5 :: a -> (a -> [a]) -> [a]
repeat5 a f = let r1 = f a 
                  r2 = flatten (map f r1)
                  r3 = flatten (map f r2)
                  r4 = flatten (map f r3)
                  r5 = flatten (map f r4)
               in r5

-- return the number of each pebble produced
jump5 :: Int -> Map.Map Int Int 
jump5 n = let pebbles = repeat5 n jump 
            in Map.fromListWith (+) [(i, 1) | i <- pebbles]

-- repeatMap :: Map a b -> (a -> Map a b) -> Map a b 
-- repeatMap m f = let mapResult = Map.map f m 
--                     mults = [(k,v*n) | (i, n) <- Map.keys m, (k,v) <- Map.toList (mapResult!!i)]
                     

jump25 :: Map.Map Int Int -> Map.Map Int Int 
jump25 j0 = let produces0 = Map.mapWithKey (\k v -> Map.map (* v) (jump5 k)) j0
                j5 = Map.foldr (Map.unionWith (+)) Map.empty produces0
                produces5 = Map.mapWithKey (\k v -> Map.map (* v) (jump5 k)) j5
                j10 = Map.foldr (Map.unionWith (+)) Map.empty produces5
                produces10 = Map.mapWithKey (\k v -> Map.map (* v) (jump5 k)) j10
                j15 = Map.foldr (Map.unionWith (+)) Map.empty produces10
                produces15 = Map.mapWithKey (\k v -> Map.map (* v) (jump5 k)) j15
                j20 = Map.foldr (Map.unionWith (+)) Map.empty produces15
                produces20 = Map.mapWithKey (\k v -> Map.map (* v) (jump5 k)) j20
                j25 = Map.foldr (Map.unionWith (+)) Map.empty produces20
                in j25 

part1 :: String -> Int
part1 content = let parsed = parse content 
                    j25 = jump25 (Map.fromList (zip parsed (repeat 1))) 
                    sums = Map.foldr (+) 0 j25
                 in sums

part2 :: String -> Int
part2 content = let parsed = parse content 
                    j25 = jump25 (Map.fromList (zip parsed (repeat 1))) 
                    j50 = jump25 j25
                    j75 = jump25 j50
                    
                    -- produces25 = Map.mapWithKey (\k v -> Map.map (* v) (jump25 k)) j25
                    -- j50 = Map.foldr (Map.unionWith (+)) Map.empty produces25
                    -- produces50 = Map.mapWithKey (\k v -> Map.map (* v) (jump25 k)) j50
                    -- j75 = Map.foldr (Map.unionWith (+)) Map.empty produces50
                    sums = Map.foldr (+) 0 j75
                 in sums


main :: IO ()
main = do
    putStrLn "Hello!"
    test_input <- readFile "test_input.txt"
    full_input <- readFile "input.txt"
    -- Tests 
    print "Tests passed!"
    unless (length (parse test_input) == 2) (error "parsing error")
    unless (3 == (length . jump5 $ 0)) (error "jump5 did not produce 4 items")
    print (jump25 (Map.fromList [(125,1)]))
    -- Part 1
    let p1Test = part1 test_input
    unless (p1Test == 55312) (error $ "wrong result for example on part 1: " ++ (show p1Test))
    let p1 = part1 full_input
    printf "Part 1: %d\n" p1

    -- Part 2
    -- let p2Test = part2 test_input
    -- unless (p2Test == 1) (error $ "wrong result for example on part 2: " ++ (show p2Test))
    let p2 = part2 full_input
    printf "Part 2: %d\n" p2