{-# LANGUAGE NamedFieldPuns #-}
module Main where

import System.Environment(getArgs)
import System.Exit(die)
import Data.Array(array, Array, listArray, (!), (//))
import Data.Maybe(listToMaybe, isNothing)
import Data.Char(ord, chr)
import Control.Monad(when)
import Data.Map(fromList, Map, (!))

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
                    , loopMap        :: Map Int Int
                    , executedCmdCt  :: Int
                    , loopStack      :: [Int]
                    } deriving (Show, Eq)

main :: IO ExecutionCtx
main = do
    filePath <- processArgs =<< getArgs
    programText <- readFile filePath
    ctx <- makeExecutionCtx $ parseBrainfuck programText
    print ctx
    runBrainfuck ctx

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

mergeCommands :: Maybe BfCommand -> [BfCommand] -> [BfCommand]
mergeCommands Nothing r = r
mergeCommands (Just l) [] = [l]
mergeCommands (Just lcmd) ((rcmd:xs)) = case (lcmd, rcmd) of
    (Add ctl, Add ctr) -> Add (ctl + ctr) : xs
    (Sub ctl, Sub ctr) -> Sub (ctl + ctr) : xs
    (SubAddress ctl, SubAddress ctr) -> SubAddress (ctl + ctr) : xs
    (AddAddress ctl, AddAddress ctr) -> AddAddress (ctl + ctr) : xs
    (lcmd, rcmd) -> [lcmd, rcmd] ++ xs

parseBrainfuck :: String -> [BfCommand]
parseBrainfuck [] = [ProgramEnd]
parseBrainfuck (x:xs) =
    let command = parseChar x in
    let rest = parseBrainfuck xs in
    mergeCommands command rest

memoryLength :: Int
memoryLength = 30000

findLoops :: [BfCommand] -> IO [(Int, Int)]
findLoops cmds = findLoops' [] [] 1 cmds
    where
        findLoops' :: [(Int, Int)] -> [Int] -> Int -> [BfCommand] -> IO [(Int, Int)]
        findLoops' loops _ _ [] = return loops
        findLoops' loops stack ix (cmd:cmds) =
            case cmd of
                LoopStart -> findLoops' loops (ix : stack) (succ ix) cmds
                LoopEnd   -> if null stack
                    then die "Loop brackets is not correct"
                    else findLoops' (loops ++ [(head stack, ix)]) (tail stack) (succ ix) cmds
                _         -> findLoops' loops stack (succ ix) cmds

makeExecutionCtx :: [BfCommand] -> IO ExecutionCtx
makeExecutionCtx program = do
    loops <- findLoops program
    let mp = fromList loops
    return $ ExecutionCtx
            { memory = array (1, memoryLength) [(i, 0) | i <- [1..memoryLength]]
            , memoryPtr = 1
            , instructions = listArray (1, length program) program
            , instructionPtr = 1
            , loopMap = mp
            , executedCmdCt = 0
            , loopStack = []
            }

changeMemoryVal :: Array Int Int -> Int -> (Int -> Int) -> Array Int Int
changeMemoryVal mem ix mapFn = mem // [(ix, mapFn (mem Data.Array.! ix))]

getCurrentMemoryCell :: ExecutionCtx -> Int
getCurrentMemoryCell ExecutionCtx { memory, memoryPtr } = memory Data.Array.! memoryPtr

getCurrentCommand :: ExecutionCtx -> BfCommand
getCurrentCommand ExecutionCtx {instructions, instructionPtr} = instructions Data.Array.! instructionPtr

advanceExecution :: ExecutionCtx -> ExecutionCtx
advanceExecution ctx = ctx {instructionPtr = succ $ instructionPtr ctx, executedCmdCt = succ $ executedCmdCt ctx }

interpretCommand :: ExecutionCtx -> BfCommand -> IO ExecutionCtx
interpretCommand ctx (Add val) = return ctx { memory = changeMemoryVal (memory ctx) (memoryPtr ctx) (\x -> mod (x + val) 256) }
interpretCommand ctx (Sub val) = return ctx { memory = changeMemoryVal (memory ctx) (memoryPtr ctx) (\x -> mod (x - val) 256) }
interpretCommand ctx (SubAddress val) =
    if memoryPtr ctx <= val then die ("Program error, attempt to underflow memory" ++ show ctx)
    else return ctx { memoryPtr = (memoryPtr ctx) - val }
interpretCommand ctx (AddAddress val) =
    if (memoryLength - memoryPtr ctx) < val then die ("Program error, attempt to overflow memory " ++ show ctx)
    else return ctx { memoryPtr = val + memoryPtr ctx}
interpretCommand ctx LoopStart =
    if getCurrentMemoryCell ctx == 0
    then return ctx { instructionPtr = loopMap ctx Data.Map.! instructionPtr ctx }
    else return ctx { loopStack = pred (instructionPtr ctx) : loopStack ctx }
interpretCommand ctx LoopEnd
  | isNothing $ listToMaybe $ loopStack ctx = die ("Program error, cycle has no open brace. " ++ show ctx)
  | otherwise = return $ ctx { instructionPtr = head $ loopStack ctx, loopStack = tail $ loopStack ctx }
interpretCommand ctx Read = do
        charVal <- getChar
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
            runBrainfuck $ advanceExecution newState