module AOC
( module Prelude
, module AOC
, module Text.Parsec
) where

import Data.Text(Text, unpack)
import Text.Parsec

import qualified Data.Text    as Text
import qualified Data.Text.IO as TextIO


type Parser = Parsec String ()

readFileText :: Text -> IO [Text]
readFileText file =
  Text.lines <$> (TextIO.readFile . unpack) file

readFileInt :: Text -> IO [Int]
readFileInt file =
  traverse (pure . textToInt) =<< readFileText file

textToInt :: Text -> Int
textToInt =
  read . Text.unpack


parselist :: Parser a
          -> [String]
          -> [a]
parselist p = either (error . show) id . mapM (parse p "")