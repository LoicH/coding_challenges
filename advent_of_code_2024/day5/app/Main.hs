module Main where

import Data.List as List
import Data.Map as Map 
import Text.Printf(printf)

-- splitOn :: (a -> Bool) -> [a] -> [a] -> ([a], [a])
-- splitOn _ [] acc = (acc, [])
-- splitOn f (x:xs) acc
--     | f x = splitOn f xs (x:acc)
--     | otherwise = (acc, x:xs)

-- split "12,3,4" into ["12", "3", "4"]
split' :: String -> Char -> String -> [String] -> [String]
split' "" _ wordAcc strAcc = (strAcc ++ [wordAcc])
split' (x:xs) c  wordAcc strAcc
    | x == c = Main.split' xs c "" (strAcc ++ [wordAcc])
    | otherwise = Main.split' xs c (wordAcc ++ [x]) strAcc

split ::  Char -> String -> [String]
split c xs = split' xs c "" []

-- rulesToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]  
-- rulesToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs  

parse :: String -> (Map.Map String [String], [[String]])
parse content = 
    let (rules, _:pagesLines) = break ((==) "") (lines content) --splitOn ((/=) "") (lines s) []
        splitRules = [(a,b) | (a,_:b) <- List.map (break ((==) '|')) rules]
        rulesMap = Map.fromListWith (\[x] xs -> List.insert x xs) $ List.map (\(k,v) -> (v,[k])) splitRules 
        pagesToProduce = List.map (Main.split ',') pagesLines
    in (rulesMap, pagesToProduce)

-- check if the list of pages is valid according to the rules
valid :: [String] -> Map.Map String [String] -> Bool
valid [x] _ = True
valid (x:xs) rules = case Map.lookup x rules of
    Just before ->  xs `noCommonSorted` before && (Main.valid xs rules) 
    Nothing -> Main.valid xs rules -- error ("No value for key " ++ x)

-- return True if no element of 'a:as' are in 'b:bs'.
-- both lists must be sorted
-- e.g. : [2,4] `noCommonSorted` [1,3] = True
noCommonSorted :: (Ord a) => [a] -> [a] -> Bool 
[] `noCommonSorted` _ = True
_ `noCommonSorted` [] = True
(a:as) `noCommonSorted` (b:bs) 
    | a < b = as `noCommonSorted` (b:bs)
    | a == b = False
    | a > b = (a:as) `noCommonSorted` bs 
 
-- part 2: we define an ordering from the rules, and we sort the invalid print orderings
isBefore :: Map.Map String [String] -> String -> String -> Ordering
isBefore rules a b = case (Map.lookup a rules, Map.lookup b rules) of
    (Nothing, Nothing) -> EQ
    (Just _, Nothing) -> GT 
    (Nothing, Just _) -> LT 
    (Just beforeA, _) -> if b `elem` beforeA then GT else LT

sumMiddles :: [[String]] -> Int 
sumMiddles l = sum (List.map (\xs -> read (getMiddle xs):: Int) l)

getMiddle :: [a] -> a
getMiddle xs = xs !! ((length xs - 1) `div` 2)


main :: IO ()
main = do
    s <- readFile "input.txt"
    -- part 1 
    -- We want to split all the "N|M" lines, so we
    -- split the lines of the input files, until we get the "" line
    let (rulesMap, pagesToProduce) = parse s
        (valids, invalids) = List.partition (\xs -> Main.valid xs rulesMap) pagesToProduce
    printf "Part 1: %d, %d\n" (length rulesMap) $ length pagesToProduce
    print . sumMiddles $ valids
    -- part 2
    print . length $ invalids
    print (List.sortBy (isBefore rulesMap) ["61", "13", "29"])
    let invalidsSorted = List.map (List.sortBy (isBefore rulesMap)) invalids
    print . sumMiddles $ invalidsSorted
