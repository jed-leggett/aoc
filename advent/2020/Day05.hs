{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import Data.Text (unpack)

import AOC

mainA :: IO ()
mainA = do
  ls <- readFileTextLines "Day05Input"
  print . maximum $ calcSeatId . unpack <$> ls

mainB :: IO ()
mainB = do
  ls <- readFileTextLines "Day05Input"
  let seats = sort $ calcSeatId . unpack <$> ls
  print . (+1) . head . tail $ filter (\ s -> notElem (s+1) seats ||  notElem (s-1) seats) seats

calcSeatId :: String -> Int
calcSeatId x = do
  let
    row = fst $ foldl partition (0, 127) (take 7 x)
    seat = fst $ foldl partition (0, 7) (drop 7 x)
  row * 8 + seat


partition :: (Int, Int) -> Char -> (Int, Int)
partition (l,h) c = do
  let s = fromRational $ (toRational (h - l)) / 2
  case c of
    'F' -> (l, h - ceiling s )
    'L' -> (l, h - ceiling s )
    'B' -> (l + ceiling s, h)
    'R' -> (l + ceiling s, h)
    _   -> (0,0)
