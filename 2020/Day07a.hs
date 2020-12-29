{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map
import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)

import AOC

type BagMap = Map.Map Color [Color]
newtype Color = Color String
  deriving (Show, Eq, Ord)

data Bag =
  Bag
  { color :: Color
  , innerBags :: [ContainedBag]
  } deriving Show

data ContainedBag =
  ContainedBag
  { clr      :: Color
  , quantity :: Int
  } deriving Show

main :: IO ()
main = do
  ls <- readFileTextLines "Day07Input"
  let
    bags = parselist parseBag $ unpack <$> ls
    bagMap = foldl addBagToMap Map.empty bags
  print . length $ countBags bagMap (Color "shiny gold")

countBags :: BagMap -> Color -> [Color]
countBags m c =
  case Map.lookup c m of
    Nothing -> [c]
    Just colors ->
      nub . (++colors) . concat $ (countBags m) <$> colors

addBagToMap :: BagMap -> Bag -> BagMap
addBagToMap m b = do
  let
    containers = innerBags b
  foldl (\ m' ib -> Map.insertWith (<>) (clr ib) [(color b)] m') m (innerBags b)

parseBag :: Parser Bag
parseBag = do
  color <- parseColor
  string " bags contain "
  containedBags <- parseContainedBags
  pure $ Bag color containedBags

parseContainedBags :: Parser [ContainedBag]
parseContainedBags = do
  noBags <- option "" $ string "no other bags"
  case noBags of
    "" -> many parseContainedBag
    _ -> pure []

parseContainedBag :: Parser ContainedBag
parseContainedBag = do
      cnt <- fmap read $ many digit
      char ' '
      clr' <- parseColor
      string " bag"
      string "s" <|> string ""
      string ", " <|> string "."
      pure $ ContainedBag clr' cnt

parseColor :: Parser Color
parseColor = fmap Color $ (many letter) <> (string " ") <> (many letter)
