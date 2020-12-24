{-# LANGUAGE OverloadedStrings #-}

module Day02 where

import Data.Foldable(traverse_)
import Data.List(elemIndex, intercalate, reverse, sort)
import Data.Map(Map(..), lookup)
import Data.Maybe(fromMaybe)
import Data.Text(Text)
import Data.Time(UTCTime(..))
import Data.Time.Clock
import Data.Time.LocalTime
import GHC.Exts(sortWith)

import qualified Data.Text    as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Map     as Map

import Debug.Trace(trace)

mainA :: IO ()
mainA = do
  ls <- (Text.splitOn ",") <$> TextIO.readFile "day02input"
  let nums = read . Text.unpack <$> ls
  traverse_ print' foo

foo :: [Int] -> Int -> [Int]
foo ls pls =
  

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs
