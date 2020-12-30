{-# LANGUAGE OverloadedStrings #-}

import Data.List (delete)
import Data.Maybe (catMaybes)

import AOC

resultA = 1504371145

mainA :: IO ()
mainA = do
  ls <- readFileIntLines "Day09Input"
  print $ findValueMissingPair ls

mainB :: IO ()
mainB = do
  ls <- readFileIntLines "Day09Input"
  print $ findContiguousValues ls []

findContiguousValues :: [Int] -> [Int] -> Int
findContiguousValues [] vals = calc vals
findContiguousValues (x:ls) vals = do
  if sum vals > resultA
    then findContiguousValues (x:ls) (tail vals)
    else
      if sum vals == resultA && length vals > 1
        then calc vals
        else do
          let vals' = vals++[x]
          if sum vals < resultA
            then findContiguousValues ls vals'
            else findContiguousValues ls (tail vals')

calc :: [Int] -> Int
calc ls' = (minimum ls') + (maximum ls')
  

findValueMissingPair :: [Int] -> Int
findValueMissingPair ls = do
  let
    (l,r) = splitAt 25 ls
    val = head r
    r' = tail r
    missingPairs = filter (==True) $ (hasPair l val) <$> l
  case missingPairs of
    [] -> val
    _  -> findValueMissingPair ((tail l) ++ r)

hasPair :: [Int] -> Int -> Int -> Bool
hasPair ls val elm =
  (val - elm) `elem` (elm `delete` ls)
