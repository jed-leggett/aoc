{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort, nub, intersect)
import Data.List.Split (splitOn)
import Data.Text (unpack)

import AOC


mainA = do
  ls <- fmap (splitOn "\n\n" . unpack) (readFileText "Day06Input")
  print . sum $ length . nub . (filter (/='\n')) <$> ls


countUnique = length . nub . (filter (/='\n'))


mainB :: IO ()
mainB = do
  ls <- fmap (splitOn "\n\n" . unpack) (readFileText "Day06Input")
  print . sum $ countSameYes <$> ls

countSameYes :: String -> Int
countSameYes s = do
 let
  foo = splitOn "\n" s
 length . last $ scanl intersect (head foo) (tail foo)
