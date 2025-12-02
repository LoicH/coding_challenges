module Main where

import Data.Char(digitToInt, intToDigit)
import qualified Data.Set as S 
import qualified Data.List as L
import qualified Data.SortedList as SL
import qualified Data.Map as M
import qualified Data.Bits as Bits
import Data.Map((!),(!?))
import Text.Printf(printf)
import Control.Monad(unless)
import Debug.Trace(trace,traceShow)

type Registers = [Int]
data Debugger =  Debugger {registers :: Registers
                ,program :: Program -- TODO Use zipper to move the instruction pointer
                } deriving (Show)

type Program = ([Int], Int, [Int]) -- Instructions to do, current instruction number, instructions done (in reverse)

forward :: Program -> Maybe ((Int, Int), Program)
forward ([], _, _) = Nothing
forward ((x:[]), _, _) = Nothing
forward ((opcode:instr:xs), pointer, breadcrumb) = Just ((opcode, instr), (xs, pointer+2, instr:opcode:breadcrumb))

goto :: Int -> Program -> Program 
goto n pgm@(todo, pointer, breadcrumb) 
    | n == pointer = pgm 
    | n < 0 = undefined 
    | n < pointer && L.length breadcrumb == 0 = undefined 
    | n > pointer && L.length todo == 0 = undefined 
    | n < pointer = let (b:bs) = breadcrumb
                     in goto n (b:todo, pointer-1, bs)
    | n > pointer = let (i:is) = todo 
                     in goto n (is, pointer+1, i:breadcrumb)

parse :: String -> Debugger
parse content = do 
    let (l1:l2:l3:l4:l5:_) = lines content
        regs = L.map (read . drop 12) [l1,l2,l3] :: [Int]
        p = (L.map digitToInt (filter ((/=) ',') . drop 9 $ l5), 0, []) :: Program
     in Debugger {registers=regs, program=p}

comboVal :: Int -> Registers -> Int 
comboVal combo (a:b:c:[]) 
    | combo <= 3 = combo 
    | combo == 4 = a 
    | combo == 5 = b 
    | combo == 6 = c 
    | otherwise = undefined
    
nextOp :: Debugger -> Maybe (Maybe String, Debugger)
nextOp dbg = do 
    let regs@[a,b,c] = registers dbg
        prog = program dbg 
     in case forward prog of 
        Nothing -> Nothing 
        Just ((instr, operand), newProg) -> case (instr, operand) of 
            (0, _) -> Just (Nothing, Debugger {registers = [a `div` 2^(comboVal operand regs), b, c], program=newProg}) -- "adv", A divided by 2^combo, truncated to int 
            (1, _) -> Just (Nothing, Debugger {registers=[a, Bits.xor b operand, c], program=newProg}) -- bxl, bitxise XOR & literal => B
            (2, _) -> Just (Nothing, Debugger {registers=[a, (comboVal operand regs) `mod` 8, c], program=newProg}) -- bst, combo modulo 8 => B
            (3, _) -> Just (Nothing, if a==0  -- Jump if A Not Zero
                                then Debugger {registers=regs, program=newProg}
                                else Debugger {registers=regs, program=goto operand prog})
            (4, _) -> Just (Nothing, Debugger {registers = [a, b `Bits.xor` c, c], program=newProg}) -- B xor C
            (5, _) -> Just (Just $ show ((comboVal operand regs)`mod` 8), Debugger {registers = regs, program=newProg}) -- out
            (6, _) -> Just (Nothing, Debugger {registers = [a, a `div` 2^(comboVal operand regs), c], program=newProg}) -- bdv
            (7, _) -> Just (Nothing, Debugger {registers = [a, b, a `div` 2^(comboVal operand regs)], program=newProg}) -- cdv
            otherwise -> undefined
                
execute :: Debugger -> [String] 
execute dbg = case nextOp dbg of 
    Nothing -> []
    Just (out, nextDbg) -> case out of 
        Nothing -> execute nextDbg
        Just o -> o : execute nextDbg

part1 :: String -> String
part1 content = do 
    let dbg = parse content 
        out = execute dbg 
     in L.foldl (++) "" $ L.intersperse "," out

outputForA :: Debugger -> Int -> [Int]
outputForA dbg newA = do 
    let [_,b,c] = registers dbg 
        p = program dbg 
        curDbg =  Debugger {registers = [newA, b,c], program=p}
        curOut = map read $ execute curDbg :: [Int]
     in curOut

outputNotEquals :: Debugger -> Int -> Bool
outputNotEquals dbg newA = do 
    let (instrs, _, _) = traceShow newA program dbg 
        curOut = outputForA dbg newA
        in curOut /= instrs

part2 :: String -> Int
part2 content = do 
    let dbg = parse content 
        lastFalse:_ = L.dropWhile (outputNotEquals dbg) [0..]
     in lastFalse


main :: IO ()
main = do
    putStrLn "Hello!"
    test_input <- readFile "test_input.txt"
    test_input2 <- readFile "test_input2.txt"
    full_input <- readFile "input.txt"
    -- Tests 
    let dbg = parse test_input2 
        -- lastFalse:_ = L.dropWhile (outputNotEquals dbg) [0..]
        newA = 117440
    print (outputForA dbg newA)
    unless (not $ outputNotEquals dbg newA) (error "Should have the same output")

    print "Tests passed!"
    

    -- Part 1
    let p1Test = part1 test_input
    unless (p1Test == "4,6,3,5,6,3,5,2,1,0") (error $ "wrong result for example on part 1: " ++ (show p1Test))
    let p1 = part1 full_input
    printf "Part 1: %s\n" p1

    -- Part 2
    let p2Test = part2 test_input2
    unless (p2Test == 117440) (error $ "wrong result for example on part 2: " ++ (show p2Test))
    print "Successful output for p2"
    let p2 = part2 full_input
    printf "Part 2: %d\n" p2