{-# LANGUAGE OverloadedStrings #-}

import Data.List(delete, elemIndex)
import Data.Text(Text, unpack)
import Data.Maybe (catMaybes)
import AOC

data Password =
  Password
  { low         :: Int
  , high        :: Int
  , limitLetter :: Char
  , password    :: String
  }

mainA :: IO ()
mainA = do
  ls <- readFileTextLines "Day02Input"
  let listOfBools = isPasswordValid <$> ((parselist parsePassword)) (unpack <$> ls)
  print . length $ filter (==True) $ listOfBools

parsePassword :: Parser Password
parsePassword = do
  low' <- many1 digit
  char '-'
  high' <- many1 digit
  char ' '
  c <- letter
  string ": "
  password' <- many1 letter
  pure $ Password (read low') (read high') c password'
  
isPasswordValid :: Password -> Bool
isPasswordValid p = do
  let letterCount = length $ filter (== limitLetter p) (password p)
  letterCount >= low p && letterCount <= high p

mainB :: IO ()
mainB = do
  ls <- readFileTextLines "Day02Input"
  let listOfBools = isPasswordValid' <$> ((parselist parsePassword)) (unpack <$> ls)
  print . length $ filter (==True) $ listOfBools

isPasswordValid' :: Password -> Bool
isPasswordValid' p = do
  let char1 = (password p) !! (low p - 1)
  let char2 = (password p) !!(high p -1)
  (char1 == (limitLetter p)) /= (char2 == (limitLetter p))
