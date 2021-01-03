{-# LANGUAGE OverloadedStrings #-}

import Data.Map (Map, fromList, toList, keys, insert)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (unpack)

import AOC

data Seat =
    UnOccupied
  | Occupied
  | Floor
  deriving (Eq, Show)

type SeatMap = Map Key Seat

mainA :: IO ()
mainA = main' False

mainB :: IO ()
mainB = main' True

main' :: Bool -> IO ()
main' isSecond = do
  ls <- (readFileTextLines "Day11Input")
  let 
    seatLists = (parseString $ manyTill parseSeat (try eof)) . unpack <$> ls
    rowLength = length seatLists
    seatList = concat seatLists
    columnLength = floor $ (toRational $ length seatList) / (toRational rowLength)
    keys = concat $ (\ f -> f <$> [0 .. columnLength-1]) <$> ( (,) <$> [0 .. rowLength-1])
    seatMap = fromList $ zip keys seatList
  print . length $ filter (\ (_,val) -> val == Occupied) . toList $ applyRules isSecond seatMap

applyRules :: Bool -> SeatMap -> SeatMap
applyRules isSecond seatMap = do
  let
    newSeatMap = foldl (applyRule isSecond seatMap) seatMap (keys seatMap)
  if seatMap == newSeatMap
    then newSeatMap
    else applyRules isSecond newSeatMap

applyRule :: Bool -> SeatMap -> SeatMap -> Key -> SeatMap
applyRule isSecond seatMap newSeatMap key = do
  let
    occupancyTolerance = if isSecond then 5 else 4
    seat         = fromMaybe Floor      $ Map.lookup key seatMap
    left'        = lookupSeat isSecond key leftOf seatMap
    topLeft'     = lookupSeat isSecond key topLeftOf seatMap
    bottomLeft'  = lookupSeat isSecond key bottomLeftOf seatMap
    right'       = lookupSeat isSecond key rightOf seatMap
    topRight'    = lookupSeat isSecond key topRightOf seatMap
    bottomRight' = lookupSeat isSecond key bottomRightOf seatMap
    top'         = lookupSeat isSecond key topOf seatMap
    bottom'      = lookupSeat isSecond key bottomOf seatMap
    adjacentSeats = [ left', topLeft', bottomLeft', right'
                    , topRight', bottomRight', top', bottom'
                    ]
    newSeat =
      case seat of
        Floor      -> Floor
        Occupied   -> if occupancyTolerance <= (length $ filter (==Occupied) adjacentSeats)
                        then UnOccupied
                        else Occupied
        UnOccupied -> if 0 == (length $ filter (==Occupied) adjacentSeats)
                        then Occupied
                        else UnOccupied
  insert key newSeat newSeatMap 

lookupSeat :: Bool -> Key -> KeyRule-> SeatMap -> Seat
lookupSeat isSecond key keyRule seatMap = do
  let
    newKey = keyRule key
    seat = fromMaybe UnOccupied $ Map.lookup newKey seatMap
  case (seat, isSecond) of
    (Floor, True) -> lookupSeat isSecond newKey keyRule seatMap
    (seat, _)  -> seat 

parseSeat :: Parser Seat
parseSeat = do
  seat <- choice [(char 'L'),( char '.')]
  case seat of
    'L' -> pure UnOccupied
    '.' -> pure Floor
    _   -> fail "oof"
