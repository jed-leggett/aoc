{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)

import AOC

data Instruction =
    Nop Int
  | Acc Int
  | Jmp Int
  deriving Show

data EndStatus =
    Killed
  | Finished
  deriving (Eq, Show)

mainA :: IO ()
mainA = do
  ls <- readFileTextLines "Day08Input"
  print . runInstructions 0 [] $ parselist parseInstruction (unpack <$> ls)

mainB :: IO ()
mainB = do
  ls <- readFileTextLines "Day08Input"
  let 
    instructions = parselist parseInstruction (unpack <$> ls)
    allInstructions = (replaceInstruction instructions) <$> [0..(length instructions - 1)]
  print . (filter (\ (a,b) -> b == Finished)) $ (runInstructions 0 []) <$> allInstructions

runInstructions :: Int -> [Int] -> [Instruction] -> (Int, EndStatus)
runInstructions currentIndex historicalIndexes instructions =
  if currentIndex `elem` historicalIndexes
    then (0, Killed)
    else 
      if length instructions == currentIndex
        then (0, Finished)
        else do
          let
            currnetInstruction = instructions !! currentIndex
            runInstructions' incc = runInstructions (currentIndex + incc) (currentIndex:historicalIndexes) instructions
          case currnetInstruction of
            Nop _   -> runInstructions' 1
            Acc num -> (\ (a,b) -> (a + num, b)) (runInstructions' 1)
            Jmp num -> runInstructions' num

parseInstruction :: Parser Instruction
parseInstruction = do
  instrc <- count 3 letter
  char ' '
  sym <- (string "-" <|> string "+")
  let neg = if sym == "-" then -1 else 1
  num <- many digit
  case instrc of
    "nop" -> pure . Nop . (*neg) $ read num
    "acc" -> pure . Acc . (*neg) $ read num
    "jmp" -> pure . Jmp . (*neg) $ read num
    _     -> fail "ops"

replaceInstruction :: [Instruction] -> Int -> [Instruction]
replaceInstruction instructions index = do
  let instruction = instructions !! index
  case instruction of
    Nop num -> replaceNth index (Jmp num) instructions
    Acc _   -> instructions
    Jmp num -> replaceNth index (Nop num) instructions