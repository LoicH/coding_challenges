module Main where

import Text.Printf(printf)
import Text.Regex.PCRE((=~))

parse :: String -> [(Int, Int)]
parse content = 
  let pat = "mul\\((\\d+),(\\d+)\\)"
      matches = content =~ pat :: [[String]]
  in [(read (m !! 1), read (m !! 2)) | m <- matches]

part1 :: String -> Int
part1 content = sum [a*b | (a,b) <- (parse content)]

valDo :: String -> Int 
valDo "" = 0
valDo s = 
    let (before, _, after) = (s =~ "don't\\(\\)" :: (String, String, String))
    in (part1 before) + (valDont after)

valDont :: String -> Int 
valDont "" = 0
valDont s = 
    let (_, _, after) = (s =~ "do\\(\\)" :: (String, String, String))
    in valDo after


main :: IO ()
main = do
  content <- readFile "input.txt"   
  printf "part 2: %d\n" $ valDo content