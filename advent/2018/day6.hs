{-# LANGUAGE OverloadedStrings #-}

import Data.List(nub, reverse, sort)
import Data.Maybe(fromMaybe)

import qualified Data.Char    as Char
import qualified Data.Map.Strict as Map
import qualified Data.Text    as Text
import qualified Data.Text.IO as TextIO

main :: IO ()
main = do
   txtCords <- Text.lines <$> TextIO.readFile "day6input"
   let 
      cordTuples = mapCord <$> txtCords
      cords = zip [1..(length cordTuples)] cordTuples
      map = puzzle2 cords
   print $ show map

puzzle1 :: [(Int, (Int, Int))] -> Int
puzzle1 cords =
   let 
      max = findBoundry cords 0 0
      newDblArr = initArray (DoubleArray Map.empty) cords
      map = mapAreas (DoubleArray Map.empty) cords max
      ls = doubleArrayToList map
      nls = reomveInfinites ls max
      infnits = nub $ fst <$> nls
      candidates = filter ((flip notElem infnits) . fst) ls
      canditIds = fst <$> candidates
      area = (head . reverse. sort) $ length . (flip filter canditIds) <$> (flip elem) <$> (:[]) <$> (nub canditIds)
   in area

puzzle2 :: [(Int, (Int, Int))] -> Int
puzzle2 cords =
   let 
      max = findBoundry cords 0 0
      newDblArr = initArray (DoubleArray Map.empty) cords
      map = mapAreas2 (DoubleArray Map.empty) cords max
      ls = doubleArrayToList map
      lessThan = filter ((<10000) .snd) ls
   in length lessThan

mapAreas :: DoubleArray -> [(Int, (Int, Int))] -> Int -> DoubleArray
mapAreas dblArr [] _ = dblArr
mapAreas dblArr (hd:ls) max =
   let
      newDblArr = mapArea dblArr hd max max max
   in mapAreas newDblArr ls max

mapArea :: DoubleArray -> (Int, (Int, Int)) -> Int -> Int -> Int -> DoubleArray
mapArea dblArr (id', (x, y)) 0 0 max = 
   let dist = (abs x) + (abs y)
   in daInsertCell dblArr 0 0 (id', dist)
mapArea dblArr (id', (x, y)) x2 y2 max =
   let
      dist = (abs (x - x2)) + (abs (y - y2))
      newDblArr = daInsertCell dblArr x2 y2 (id', dist)
   in
      if x2 == 0 then
          mapArea newDblArr (id', (x, y)) (max) (y2 - 1) max
      else
         mapArea newDblArr (id', (x, y)) (x2 - 1) (y2) max

mapCord :: Text.Text -> (Int, Int)
mapCord txtCord =
   let (x:(y:[])) = Text.splitOn ", " txtCord
   in (read (Text.unpack x)+1 :: Int, read (Text.unpack y)+1 :: Int)

findBoundry :: [(Int, (Int, Int))] -> Int -> Int -> Int
findBoundry [] x y = (max x y)+1
findBoundry ((_,(x,y)):ls) maxX maxY =
   findBoundry ls (max x maxX) (max y maxY)


data DoubleArray =
   DoubleArray 
   { arr :: Map.Map Int (Map.Map Int (Int, Int))
   } deriving Show

initArray :: DoubleArray -> [(Int, (Int, Int))] -> DoubleArray
initArray dblArr [] = dblArr
initArray dblArr ((id', (x, y)):ls) =
   let
      newDblArr = daInsertCell dblArr x y (id', 0)
   in
      initArray newDblArr ls

daInsertCell :: DoubleArray -> Int -> Int -> (Int, Int) -> DoubleArray
daInsertCell dblArr row col (id',dist) =
   let
      columns = fromMaybe Map.empty (Map.lookup row $ arr dblArr)
      newDblArr val = DoubleArray $ Map.insert row (Map.insert col val columns) (arr dblArr)
   in
      case Map.lookup col columns of
         Nothing -> newDblArr (id',dist)
         Just (_, 0) -> dblArr
         Just (id2, dist2) ->
            if dist2 < dist then
               dblArr
            else
               if dist2 > dist then
                  newDblArr (id',dist)
               else
                  newDblArr(0, dist)

doubleArrayToList :: DoubleArray -> [(Int, Int)]
doubleArrayToList dblArr =
   (concat $ Map.elems <$> (Map.elems $ arr dblArr))

reomveInfinites :: [a] -> Int -> [a]
reomveInfinites ls max =
   let
      (hd, lsx) = splitAt (max) ls
      (bdy, tl) = splitAt ((length lsx) - max) lsx
      edges = (reomveInfinites' bdy (max + 1))
   in
      (hd ++ edges ++ tl)

reomveInfinites' :: [a] -> Int -> [a]
reomveInfinites' [] _ = []
reomveInfinites' ls max =  (++) (reomveInfinites' (drop max ls) max) (take 2 ls)




mapAreas2 :: DoubleArray -> [(Int, (Int, Int))] -> Int -> DoubleArray
mapAreas2 dblArr [] _ = dblArr
mapAreas2 dblArr (hd:ls) max =
   let
      newDblArr = mapArea2 dblArr hd max max max
   in mapAreas2 newDblArr ls max

mapArea2 :: DoubleArray -> (Int, (Int, Int)) -> Int -> Int -> Int -> DoubleArray
mapArea2 dblArr (id', (x, y)) 0 0 max = 
   let dist = (abs x) + (abs y)
   in daInsertCell2 dblArr 0 0 (id', dist)
mapArea2 dblArr (id', (x, y)) x2 y2 max =
   let
      dist = (abs (x - x2)) + (abs (y - y2))
      newDblArr = daInsertCell2 dblArr x2 y2 (id', dist)
   in
      if x2 == 0 then
          mapArea2 newDblArr (id', (x, y)) (max) (y2 - 1) max
      else
         mapArea2 newDblArr (id', (x, y)) (x2 - 1) (y2) max

daInsertCell2 :: DoubleArray -> Int -> Int -> (Int, Int) -> DoubleArray
daInsertCell2 dblArr row col (id',dist) =
   let
      columns = fromMaybe Map.empty (Map.lookup row $ arr dblArr)
      newDblArr val = DoubleArray $ Map.insert row (Map.insert col val columns) (arr dblArr)
   in
      case Map.lookup col columns of
         Nothing -> newDblArr (id',dist)
         Just (id2, dist2) -> newDblArr (id',dist+dist2)





