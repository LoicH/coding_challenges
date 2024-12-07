module Main where

import Data.List as List
import Text.Printf(printf)

occurrences :: String -> String -> Int
occurrences _ "" = 0
occurrences needle haystack --can do multiple needles or reverse needle also
    | ln > lh = 0
    | take ln haystack == needle = succ . occurrences needle $ tail haystack
    | otherwise = occurrences needle $ tail haystack
    where ln = length needle 
          lh = length haystack 


-- Number of times we find "needle" and reverse("needle") in the lines of ls
occurrencesRevLines :: String -> [String] -> Int 
occurrencesRevLines needle ls = 
        let c = sum . map (occurrences needle) $ ls
            cRev = sum . map (occurrences . reverse $ needle) $ ls
        in c+cRev

-- reverse :: [a] -> [a] 
-- reverse = foldl (flip (:)) []

antiDiagonals :: [String] -> [String]
antiDiagonals ls = [[ls !! i !! j | i <- [h-1, h-2..0], j <- [0..w-1], i+j == s] | s <- [0..w+h-1]]
    where h = length ls
          w = length . head $ ls

diagonals :: [String] -> [String]
diagonals ls = [[ls !! (start+k) !! k | k <- [0..w-1], 0<=start+k && start+k < h] | start <- [-h+1..w-1]]
    where h = length ls
          w = length . head $ ls

{- To find the two "MAS" in the shape of an X,
we can iterate over every character of the lines to find an 'A', which will be the center of the X
then check around the A (in every "corner") if we find "MSSM" which corresponds to:
    M.S
    .A.
    M.S
or we find "MMSS", or "SMMS" or "SSMM"
-}
-- Check if there is a cross centered around these coordinates
isMasCross :: [String] -> Int -> Int -> Bool
isMasCross ls i j = do
    (ls !! i !! j) == 'A' && (co `elem` ["MSSM", "MMSS", "SMMS", "SSMM"])
    where co = corners ls i j

corners :: [String] -> Int -> Int -> String
corners ls i j = [ls !! (i-1) !! (j-1), 
                    ls !! (i-1) !! (j+1),
                    ls !! (i+1) !! (j+1),
                    ls !! (i+1) !! (j-1)]


main :: IO ()
main = do
    s <- readFile "input.txt"
    {- part 1
    -- horizontally
    let hor = occurrencesRevLines  "XMAS" . lines $ s -- 5
        -- vertically : transpose
        vert = occurrencesRevLines  "XMAS"  $ List.transpose . lines $ s -- 3
        -- diagonals    
        diag = occurrencesRevLines  "XMAS"  $ diagonals . lines $ s -- 5
        -- antiDiagonals    
        antiDiag = occurrencesRevLines  "XMAS"  $ antiDiagonals . lines $ s -- 5
    printf "%d\n" $ hor + vert + diag + antiDiag
    -}
    -- part 2
    let ls = lines s 
        h = length ls
        w = length . head $ ls 
        bools = [1 | i <- [1..h-2], j <- [1..w-2], isMasCross (lines s) i j]
        countTrue = length bools
    printf "Part 2: %d\n" countTrue
