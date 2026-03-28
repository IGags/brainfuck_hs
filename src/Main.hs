{-# LANGUAGE NamedFieldPuns #-}
module Main where

import System.Environment(getArgs)
import System.Exit(die)
import Data.Array(array, Array, listArray, (!), (//))
import Data.Maybe(listToMaybe, isNothing)
import Data.Char(ord, chr)
import Control.Monad(when)

data BfCommand =
      Add Int
    | Sub Int
    | SubAddress Int
    | AddAddress Int
    | LoopStart
    | LoopEnd
    | Write
    | Read
    | ProgramEnd
    deriving (Show, Eq)

data ExecutionCtx = ExecutionCtx
                    { memory         :: Array Int Int
                    , memoryPtr      :: Int
                    , instructions   :: Array Int BfCommand
                    , instructionPtr :: Int
                    , loopStack      :: [Int]
                    , executedCmdCt  :: Int
                    } deriving (Show, Eq)

main :: IO ExecutionCtx
main = do
    filePath <- processArgs =<< getArgs
    programText <- readFile filePath
    runBrainfuck $ makeExecutionCtx $ parseProgram programText

argErrorText :: String
argErrorText = "Invalid args count, please parse single arg wich is brainfuck file path."

processArgs :: [String] -> IO String
processArgs [] = die argErrorText
processArgs [x] = return x
processArgs (_:_) = die argErrorText

parseChar :: Char -> Maybe BfCommand
parseChar ch = case ch of
    '+' -> Just $ Add 1
    '-' -> Just $ Sub 1
    '<' -> Just $ SubAddress 1
    '>' -> Just $ AddAddress 1
    '[' -> Just LoopStart
    ']' -> Just LoopEnd
    '.' -> Just Write
    ',' -> Just Read
    _   -> Nothing

parseProgram :: [Char] -> [BfCommand]
parseProgram = foldr fld [ProgramEnd]
    where
        fld :: Char -> [BfCommand] -> [BfCommand]
        fld char arr = case parseChar char of
            Nothing -> arr
            Just cmd -> cmd : arr

memoryLength :: Int
memoryLength = 30000

makeExecutionCtx :: [BfCommand] -> ExecutionCtx
makeExecutionCtx program =
    ExecutionCtx
    { memory = array (1, memoryLength) [(i, 0) | i <- [1..memoryLength]]
    , memoryPtr = 1
    , instructions = listArray (1, length program) program
    , instructionPtr = 1
    , loopStack = []
    , executedCmdCt = 0
    }

changeMemoryVal :: Array Int Int -> Int -> (Int -> Int) -> Array Int Int
changeMemoryVal mem ix mapFn = mem // [(ix, mapFn (mem ! ix))]

getCurrentMemoryCell :: ExecutionCtx -> Int
getCurrentMemoryCell ExecutionCtx { memory, memoryPtr } = memory ! memoryPtr

getCurrentCommand :: ExecutionCtx -> BfCommand
getCurrentCommand ExecutionCtx {instructions, instructionPtr} = instructions ! instructionPtr

advanceExecution :: ExecutionCtx -> ExecutionCtx
advanceExecution ctx = ctx {instructionPtr = succ $ instructionPtr ctx, executedCmdCt = succ $ executedCmdCt ctx }

interpretCommand :: ExecutionCtx -> BfCommand -> IO ExecutionCtx
interpretCommand ctx (Add _)
  | getCurrentMemoryCell ctx == 255 = return ctx { memory = changeMemoryVal (memory ctx) (memoryPtr ctx) (const 0) }
  | otherwise = return ctx { memory = changeMemoryVal (memory ctx) (memoryPtr ctx) succ }
interpretCommand ctx (Sub _)
  | getCurrentMemoryCell ctx == 0 = return ctx { memory = changeMemoryVal (memory ctx) (memoryPtr ctx) (const 255) }
  | otherwise = return ctx { memory = changeMemoryVal (memory ctx) (memoryPtr ctx) pred }
interpretCommand ctx (SubAddress _) =
    if memoryPtr ctx == 0 then die ("Program error, attempt to underflow memory" ++ show ctx)
    else return ctx { memoryPtr = pred $ memoryPtr ctx }
interpretCommand ctx (AddAddress _) =
    if memoryPtr ctx == memoryLength then die ("Program error, attempt to overflow memory " ++ show ctx)
    else return ctx { memoryPtr = succ $ memoryPtr ctx}
interpretCommand ctx LoopStart = return ctx { loopStack = instructionPtr ctx : loopStack ctx}
interpretCommand ctx LoopEnd
  | isNothing $ listToMaybe $ loopStack ctx = die ("Program error, cycle has no open brace. " ++ show ctx)
  | getCurrentMemoryCell ctx == 0 = return ctx { loopStack = tail $ loopStack ctx, executedCmdCt = succ $ executedCmdCt ctx }
  | otherwise = return ctx { instructionPtr = head $ loopStack ctx, executedCmdCt = succ $ executedCmdCt ctx }
interpretCommand ctx Read = do
        charVal <- getChar
        when (charVal /= '\n') $ putChar '\n'
        return ctx { memory = changeMemoryVal (memory ctx) (memoryPtr ctx) (const $ ord charVal) }
interpretCommand ctx Write = do
    putChar $ chr $ getCurrentMemoryCell ctx
    return ctx
interpretCommand ctx ProgramEnd = return ctx

runBrainfuck :: ExecutionCtx -> IO ExecutionCtx
runBrainfuck ctx = case getCurrentCommand ctx of
    ProgramEnd -> return ctx
    _ ->
        do
            newState <- interpretCommand ctx (getCurrentCommand ctx)
            --print newState
            runBrainfuck $ advanceExecution newState