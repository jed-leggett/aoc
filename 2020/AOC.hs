{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module AOC
( module Prelude
, module AOC
, module Text.Parsec
, module Text.Parsec.Error
) where

import Control.Arrow (left)
import Data.Hashable (Hashable)
import Data.List( elemIndex)
import Data.Maybe (fromJust)
import Data.Text(Text, unpack)
import GHC.Generics (Generic)
import Text.Parsec
import Text.Parsec.Error

import qualified Data.Text    as Text
import qualified Data.Text.IO as TextIO


type Parser = Parsec String ()


readFileText :: Text -> IO Text
readFileText =
  (TextIO.readFile . unpack)

readFileTextLines :: Text -> IO [Text]
readFileTextLines =
  fmap Text.lines <$> readFileText

readFileIntLines :: Text -> IO [Int]
readFileIntLines file =
  traverse (pure . textToInt) =<< readFileTextLines file

textToInt :: Text -> Int
textToInt =
  read . Text.unpack

parselist :: Parser a
          -> [String]
          -> [a]
parselist p = either (error . show) id . mapM (parse p "")

parselistOrError :: Parser a
                 -> [String]
                 -> Either String [a]
parselistOrError p = (left (show)) . mapM (parse p "")

parseString :: Parser a
            -> String
            -> a
parseString p = (either (error . show) id) . (parse p "")

parseStringOrError :: Parser a
                   -> String
                   -> Either String a
parseStringOrError p = (left (show)) . (parse p "")

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

safeIndex :: [a] -> Int -> Maybe a
safeIndex ls i =
  if i < 0 || i >= length ls
    then Nothing
    else Just $ ls !! i

type Key = (Int, Int)
type KeyRule = Key -> Key

leftOf :: KeyRule
leftOf (x,y) = (x-1,y)

topLeftOf :: KeyRule
topLeftOf (x,y) = (x-1,y+1)

bottomLeftOf :: KeyRule
bottomLeftOf (x,y) = (x-1,y-1)

rightOf :: KeyRule
rightOf (x,y) = (x+1,y)

topRightOf :: KeyRule
topRightOf (x,y) = (x+1,y+1)

bottomRightOf :: KeyRule
bottomRightOf (x,y) = (x+1,y-1)

topOf :: KeyRule
topOf (x,y) = (x,y+1)

bottomOf :: KeyRule
bottomOf (x,y) = (x,y-1)
