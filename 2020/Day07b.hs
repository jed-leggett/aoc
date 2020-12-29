{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)

import AOC

type BagMap = Map.Map Color Bag
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
    bagMap = Map.fromList $ (\ b-> (color b, b)) <$> bags
    myBag = fromJust $ Map.lookup (Color "shiny gold") bagMap
  print $ countBags bagMap myBag

countBags :: BagMap -> Bag -> Int
countBags m bag = do
  let
    tuples = mkInnerAndBagTuples <$> innerBags bag
  case tuples of
    [] -> 0
    _ -> foldl (+) 0 (countAndMultiply <$> tuples)
  where
    mkInnerAndBagTuples :: ContainedBag -> (ContainedBag, Bag)
    mkInnerAndBagTuples = (\ ib -> (ib, fromJust . (flip Map.lookup m) . clr $ ib))
    countAndMultiply :: (ContainedBag, Bag) -> Int
    countAndMultiply (ib, b) = do
      let
        qty = quantity ib
        cnt = countBags m b
        mult = qty * cnt
      mult + qty

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
