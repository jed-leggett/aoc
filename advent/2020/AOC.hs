module AOC
( module Prelude
, module AOC
, module Text.Parsec
, module Text.Parsec.Error
) where

import Data.Text(Text, unpack)
import Text.Parsec
import Text.Parsec.Error
import Control.Arrow (left)

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
