{-# LANGUAGE OverloadedStrings #-}

import Data.Function (on)
import Data.List(groupBy, nub, partition, reverse, sort, sortBy)
import Data.Maybe(fromMaybe)
import Data.Ord (comparing)

import qualified Data.Char    as Char
import qualified Data.Map.Strict as Map
import qualified Data.Text    as Text
import qualified Data.Text.IO as TextIO

import Debug.Trace(trace)

main :: IO ()
main = do
   inputs <- Text.lines <$> TextIO.readFile "day7input"
   let 
      nodes = mapNodes . Text.unpack <$> inputs

   print $ buildGraph $ buildNodes nodes

mapNodes :: [Char] -> (Char, Char)
mapNodes ('S':'t':'e':'p':' ':a:' ':'m':'u':'s':'t':' ':'b':'e':' ':'f':'i':'n':'i':'s':'h':'e':'d':' ':'b':'e':'f':'o':'r':'e':' ':'s':'t':'e':'p':' ':b:_) =
   (a, b)

buildGraph :: [(Char, [Char])] -> String
buildGraph nodes =
   let
      (firstNodes, rest) = partition ((flip notElem (concat $ snd <$> nodes)) . fst) nodes
   in
      buildGraph' firstNodes rest

buildGraph' :: [(Char, [Char])] -> [(Char, [Char])] -> String
buildGraph' [] [] = ""
buildGraph' [] nodes = fst <$> nodes
buildGraph' ((node,edges):tl) nodes =
   let
      (nextNodes, rest) = partition (flip elem edges . fst) nodes
      graph = [node] ++ buildGraph' nextNodes rest
      filteredNodes = filter (flip elem graph . fst) rest
   in
      graph ++ buildGraph' tl filteredNodes


buildNodes :: [(Char, Char)] -> [(Char, [Char])]
buildNodes edges = 
   let
      sortAndGroupEdgesByNode = (groupBy ((==) `on` fst) . sortBy (comparing fst))
      foldEdges = (\ n -> (fst $ head n, snd <$> n))
   in
      foldEdges <$> sortAndGroupEdgesByNode edges


