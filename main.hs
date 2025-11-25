module Main where

import Data.Sequence ( Seq (Empty), lookup, index )
import Data.Sequence qualified (null, drop, take, (><))

main :: IO ()
main = print $ show $ parseBrainfuck ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.>>>++++++++[<++++>-]<.>>>++++++++++[<+++++++++>-]<---.<<<<.+++.------.--------.>>+.>++++++++++."

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

shitftSeq :: Seq a -> Int -> Seq a
shitftSeq Empty _ = Empty
shitftSeq sq ct = Data.Sequence.drop ct sq Data.Sequence.>< Data.Sequence.take ct sq

interpretBrainfuckProgram :: Seq BfCommand -> Seq Int -> IO ()
interpretBrainfuckProgram program memory
    | Data.Sequence.null program = putStrLn "Program is empty"
    | Data.Sequence.null memory = putStrLn "Memory is empty"
    | program `index` 0 == ProgramEnd = putStrLn "Execution complete!"
    | otherwise = case (program `index` 0, memory `index` 0) of
    (Add ct, val) -> putStrLn "huy"