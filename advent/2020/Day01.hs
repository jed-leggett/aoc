{-# LANGUAGE OverloadedStrings #-}

import Data.List(delete)
import Data.Maybe (catMaybes)

import AOC


mainA :: IO ()
mainA = do
  ls <- readFileIntLines "Day01Input"
  print . mconcat $ (findPair ls) <$> ls

findPair :: [Int] -> Int -> [Int]
findPair ls a =
  let
    listMinusA = delete a ls
  in
    catMaybes $ checkIfValue . (:[a]) <$> listMinusA

mainB :: IO ()
mainB = do
  ls <- readFileIntLines "Day01Input"
  print . mconcat $ (findValue ls) <$> ls

findValue :: [Int] -> Int -> [Int]
findValue ls a =
  let
    listMinusA = delete a ls
  in
    mconcat $ (findValue' listMinusA a) <$> listMinusA

findValue' :: [Int] -> Int -> Int -> [Int]
findValue' ls a b =
  let
    listMinusB = delete b ls
  in
    catMaybes $ checkIfValue . (:[a, b]) <$> listMinusB

checkIfValue :: [Int] -> Maybe Int
checkIfValue vals =
  if sum vals == 2020
    then Just (product vals)
    else Nothing

mainB2 :: IO ()
mainB2 = do
  ls <- readFileIntLines "Day01Input"
  print . mconcat $ (findValue ls) <$> ls
