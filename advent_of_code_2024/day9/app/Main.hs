
module Main where

import Data.List as List
import Data.Map as Map 
import Data.Set as Set 
import Data.Char(digitToInt, intToDigit)
import Text.Printf(printf)
import Control.Monad 
import Debug.Trace(trace)

type Block = Either File Free

showBlock :: Block -> String 
showBlock b = case b of 
    Left f -> show f 
    Right f -> show f

showBlocks :: [Block] -> String
showBlocks [] = ""
showBlocks (x:xs) = (showBlock x) ++ (showBlocks xs)

data File = File {number:: Int
                , fsize::Int} deriving (Eq)

data Free = Free {space::Int} deriving (Eq)

instance Show File where 
    show f = List.take (fsize f) $ repeat . intToDigit . number $ f

instance Show Free where 
    show f = List.take (space f) $ repeat '.'

-- The list of blocks (queue), a stack of files, and the total files size
parse' :: String -> [Block] -> [File] -> Int -> Int -> Bool -> ([Block], [File], Int)
parse' content blocks files count totalFileSize isBlock = case (content, isBlock) of 
    ("", _) -> (blocks, files, totalFileSize)
    (x:xs, True) -> let b = File {number=count, fsize=digitToInt x} in parse' xs (blocks ++ [Left b]) (b:files) (succ count) (totalFileSize + digitToInt x) False
    (x:xs, False) -> let f = Free {space=digitToInt x} in parse' xs (blocks ++ [Right f]) files count totalFileSize True

parse :: String -> ([Block], [File], Int)
parse content = parse' content [] [] 0 0 True

fill :: [Block] -> [File] -> Int -> [File]
fill blocks files remainFileSize = case {- trace ("remain= " ++ show remainFileSize)-} blocks of 
    [] -> []
    Left file : xs -> if fsize file >= remainFileSize 
                      then {- trace ("Overflow: " ++ show (fsize file) ++ " / " ++ show remainFileSize)-}[File {number=number file, fsize=remainFileSize} ]
                      else file : (fill xs files (remainFileSize-(fsize file)))
    Right free : xs -> if length files == 0
        then []
        else let x = min (space free) remainFileSize
                 (f:fstack) = files 
                 fs = fsize f 
                 fn = number f
             in if fs == x 
                then f : (fill xs fstack (remainFileSize-fs))
                else if fs < x
                then f : (fill (Right Free {space=x-fs}:xs) fstack (remainFileSize-fs))
                else File {number=fn, fsize=x} : (fill xs (File {number=fn, fsize=fs-x}:fstack) (remainFileSize-x))

checksum' :: Int -> [File] -> Int
checksum' _ [] = 0
checksum' pos (File {number=n, fsize=s}:fs) = do
    let blockVal = sum $ List.map (*n) [pos..pos+s-1]
     in blockVal + checksum' (pos+s) fs
        

checksum :: [File] -> Int
checksum = checksum' 0

part1 :: String -> Int
part1 content = do
    let (b,f,n) = parse content
        filled = fill b f n 
        check = checksum filled
     in check


-- Part 2
replace :: (Eq a) => a -> a -> [a] -> [a]
replace a b (x:xs) = if a==x
                     then b:xs
                     else x:(replace a b xs)


insert :: File -> [Block] -> [Block]
insert finsert@(File {number=n, fsize=s}) blocks = case {-trace ("insert n="++show n++" size="++show s ++"head="++show (head blocks)++"\n")-} blocks of
    [] -> error "Shouldn't have an empty list?"
    (Left f:bs) -> if f==finsert  -- We reached the original file we wanted to insert
                  then blocks
                  else (Left f) : (Main.insert finsert bs)
    (r@(Right Free {space=x}):bs) -> if {-trace ("Free= " ++ show x ++ " to insert " ++ show s ++ "\n")-} s==x 
                                  then Left File {number=n, fsize=s} : (replace (Left finsert) (Right Free {space=s}) bs)
                                  else if s < x 
                                  then Left File {number=n, fsize=s} : Right Free {space=x-s} : (replace (Left finsert) (Right Free {space=s}) bs)
                                  else r: (Main.insert finsert bs)

fill2 :: [Block] -> [File] -> [Block]
fill2 blocks [] = blocks 
fill2 blocks (f:fs) = fill2 (Main.insert f blocks) fs

checksum2' :: [Block] -> Int -> Int 
checksum2' [] _ = 0
checksum2' (b:bs) pos = case b of 
    Right Free {space=x} -> checksum2' bs (pos+x)
    Left File {number=n, fsize=s} -> do
        let blockVal = sum $ List.map (*n) [pos..pos+s-1]
            in blockVal + checksum2' bs (pos+s)

part2 :: String -> Int
part2 content = do
    let (b,f,_) = parse content 
        filled = fill2 b f
        check = checksum2' filled 0 
     in check

main :: IO ()
main = do
    test_input <- readFile "test_input.txt"
    full_input <- readFile "input.txt"
    -- Tests 
    let (b,f,_) = parse test_input -- "123451214" -- 
    -- print "b"
    -- print . showBlocks $ b
    -- print "f"
    -- print f 
    -- print "n"
    -- print n
    -- print "fill"
    let filled = fill2 b f 
    print "filled"
    print . length $ filled
    print filled
    let c = checksum2' filled 0
    print "checksum2"
    print c
    -- Part 1
    -- let p1Test = part1 test_input
    -- unless (p1Test == 1928) (error $ "wrong result for example on part 1: " ++ (show p1Test))
    -- let p1 = part1 full_input
    -- printf "Part 1: %d\n" p1

    -- Part 2
    let p2Test = part2 test_input
    unless (p2Test == 2858) (error $ "wrong result for example on part 2: " ++ (show p2Test))
    let p2 = part2 full_input :: Int
    printf "Part 2: %d\n" p2
