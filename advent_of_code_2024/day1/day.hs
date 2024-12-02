import Data.List (sort)
import Text.Printf(printf)

-- Used to split [a,b,c,d] into ([a,c], [b,d])
splitTwo :: [String] -> [Int] -> [Int] -> ([Int], [Int])
splitTwo [] l r = (l, r)
splitTwo (a:b:xs) l r = splitTwo xs (read a :l) (read b :r)

count :: Int -> [Int] -> Int 
count _ [] = 0
count a (x:xs) = (if a==x then 1 else 0) + (count a xs)

-- We use the fact that the two lists are sorted for part 2
similarity :: [Int] -> [Int] -> Int
similarity _ [] = 0
similarity [] _ = 0
similarity (a:as) (b:bs) = if a==b then (a + (similarity (a:as) bs)) else (if a>b then (similarity (a:as) bs) else (similarity as (b:bs)))

main :: IO ()
main = do
  content <- readFile "test_input.txt"
  let (a, b)    = splitTwo (words $ content) [] []
      (aSorted, bSorted) = (sort a, sort b)
      r = sum [abs (x-y) | (x,y) <- (zip aSorted bSorted)]
      s = similarity aSorted bSorted
  printf "Part 1: %d, Part 2: %d\n" r s
