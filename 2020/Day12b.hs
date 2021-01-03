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
    MoveWaypoint Heading Int
  | RotateWaypoint Direction Int
  | MoveShip Int
  deriving Show

data Ship =
  Ship
  { location :: (Int,Int)
  , waypoint :: (Int,Int)
  } deriving Show

mainB :: IO ()
mainB = do
  ls <- (readFileTextLines "Day12Input")
  let instructions = (parseString parserInstruction) . unpack <$> ls
  print . (\ (a,b) -> abs a + abs b) . location $ foldl applyInstruction (Ship (0,0) (10,1)) instructions

-- 18531 too low

applyInstruction :: Ship -> Instruction -> Ship
applyInstruction ship instruction =
  case instruction of
    (MoveWaypoint heading num)  -> moveWaypoint ship heading num
    (RotateWaypoint dirc num) -> rotateWaypoint ship dirc num
    (MoveShip num)            -> moveShip ship num

moveWaypoint :: Ship -> Heading -> Int -> Ship
moveWaypoint s h n =
  s { waypoint = 
      case h of
        North -> (\ (x,y) -> (x,y+n)) (waypoint s)
        South -> (\ (x,y) -> (x,y-n)) (waypoint s)
        East  -> (\ (x,y) -> (x+n,y)) (waypoint s)
        West  -> (\ (x,y) -> (x-n,y)) (waypoint s)
    }

moveShip :: Ship -> Int -> Ship
moveShip s n = do
  let
    (sx,sy) = location s
    (wx,wy) = waypoint s
    newLoc = (sx+(wx*n),sy+(wy*n))
  s { location = newLoc }

rotateWaypoint :: Ship -> Direction -> Int -> Ship
rotateWaypoint s _ 0 = s
rotateWaypoint s d n = do
  let
    (x,y) = waypoint s
    newWaypoint = case d of
            Left'  -> (negate y, x)
            Right' -> (y,negate x)
  rotateWaypoint (s {waypoint = newWaypoint}) d (n-90)

parserInstruction :: Parser Instruction
parserInstruction = do
  l <- letter
  num <- fmap read $ manyTill digit (try eof)
  case l of
    'N' -> pure $ MoveWaypoint North num
    'S' -> pure $ MoveWaypoint South num
    'E' -> pure $ MoveWaypoint East num
    'W' -> pure $ MoveWaypoint West num
    'L' -> pure $ RotateWaypoint Left' num
    'R' -> pure $ RotateWaypoint Right' num
    'F' -> pure $ MoveShip num
    _   -> fail "oof"