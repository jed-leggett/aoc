{-# LANGUAGE OverloadedStrings #-}

module Day01 where

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
  ls <- Text.lines <$> TextIO.readFile "day01input"
  print . sum $ calculateFuelA . read . Text.unpack <$> ls

mainB :: IO ()
mainB = do
  ls <- Text.lines <$> TextIO.readFile "day01input"
  print . sum $ calculateFuelB . read . Text.unpack <$> ls

calculateFuelA :: Int -> Int
calculateFuelA mass =
  (mass `div` 3 :: Int) - 2

calculateFuelB :: Int -> Int
calculateFuelB mass =
  let fuel = (mass `div` 3 :: Int) - 2
  in
    if trace (show fuel) $ fuel > 0
    then fuel + calculateFuelB fuel
    else 0
