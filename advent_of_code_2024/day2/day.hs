import Data.List (sort)
import Text.Printf(printf)

{- isSafe = toujours croissant/décroissant dans le même sens, et écart >= 1 et <= 3

isSafe [3,4] 1 => (1, 1)
isSafe [0, 4] 1 => (0, 1)
isSafe [1, 0, 2] => Oui
isSafe [1, _, 2] => Oui
isSafe [0, 1, _, 2] => Oui
isSafe [1, 1, _, 2] => Non
isSafe [2, 1, 0, 2] => Oui
isSafe [3, 1, 0, 2] => Oui

| a:b:[], chances -> ((1 <= écart <= 3 ? chance, sinon chances-1), b-a)
| a:b:xs, chances -> (safe, sign) = (isSafe (b:xs) chances). si safe & chances > 0 => (True, sign)


-}

isSafe' :: [Int] -> Int -> Bool
isSafe' (x:[]) _ = True
isSafe' (a:b:xs) sign = if (1 <= (abs (b-a))) && ((abs (b-a)) <= 3) && ((b-a) * sign > 0) then (isSafe' (b:xs) sign) else False

isSafe :: [Int] -> Bool
isSafe (a:b:xs) = isSafe' (a:b:xs) (b-a)

parse :: String -> [[Int]]
parse content = map (map readInt) (map words (lines content))




readInt :: String -> Int
readInt = read

main :: IO ()
main = do
  content <- readFile "input.txt"
  let a = isSafe [1,2,3]
      b = length $ filter isSafe $ parse content
  printf "part 1: %d\n" b
