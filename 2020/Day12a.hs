{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (unpack)

import AOC

data Heading =
    North
  | South
  | East
  | West
  deriving Show

data Direction =
    Left'
  | Right'
  deriving (Eq, Show)

data Instruction =
    Move Heading Int
  | ChangeHeading Direction Int
  | MoveForward Int
  deriving Show

data Ship =
  Ship
  { location :: (Int,Int)
  , heading :: Heading
  } deriving Show

mainA :: IO ()
mainA = do
  ls <- (readFileTextLines "Day12Input")
  let instructions = (parseString parserInstruction) . unpack <$> ls
  print . (\ (a,b) -> abs a + abs b) . location $ foldl applyInstruction (Ship (0,0) East) instructions


applyInstruction :: Ship -> Instruction -> Ship
applyInstruction ship instruction =
  case instruction of
    (Move North num)         -> moveShip ship (0,num)
    (Move South num)         -> moveShip ship (0,negate num)
    (Move East num)          -> moveShip ship (negate num,0)
    (Move West num)          -> moveShip ship (num,0)
    (ChangeHeading dirc num) -> ship { heading = updateHeading dirc (heading ship) num }
    (MoveForward num)        -> applyInstruction ship (Move (heading ship) num)

moveShip :: Ship -> (Int,Int) -> Ship
moveShip s (x,y) = do
  let newLoc = (\ (x',y') -> (x'+x,y'+y)) (location s)
  s { location = newLoc }

updateHeading :: Direction -> Heading -> Int -> Heading
updateHeading _ currentHeading 0 = currentHeading
updateHeading direction currentHeading degrees = do
  let newHeading =
       case currentHeading of
        North -> if Right' == direction then East else West
        South -> if Right' == direction then West else East
        East  -> if Right' == direction then South else North
        West  -> if Right' == direction then North else South
  updateHeading direction newHeading (degrees-90)

parserInstruction :: Parser Instruction
parserInstruction = do
  l <- letter
  num <- fmap read $ manyTill digit (try eof)
  case l of
    'N' -> pure $ Move North num
    'S' -> pure $ Move South num
    'E' -> pure $ Move East num
    'W' -> pure $ Move West num
    'L' -> pure $ ChangeHeading Left' num
    'R' -> pure $ ChangeHeading Right' num
    'F' -> pure $ MoveForward num
    _   -> fail "oof"