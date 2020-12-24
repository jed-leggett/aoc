{-# LANGUAGE OverloadedStrings #-}
module Day03 where

import Data.Text (Text, unpack)

import AOC

mainA :: IO ()
mainA = do
  ls <- readFileTextLines "Day03Input"
  print $ foo ls 0 0 (3,1)

foo :: [Text] -> Int -> Int -> (Int, Int) -> Int
foo ls x y (xv, yv) =  do
  let x' = mod x 31
  if y+yv >= length ls
    then 0
    else
      case (unpack $ (ls !! y)) !! x' of
        '.' -> foo ls (x+xv) (y+yv) (xv, yv)
        '#' -> foo ls (x+xv) (y+yv) (xv, yv) + 1
        _   -> 0

mainB :: IO ()
mainB = do
  ls <- readFileTextLines "Day03Input"
  let vectors = [(1,1),(3,1),(5,1),(7,1),(1,2)]
  print . product $ (foo ls 0 0) <$> vectors