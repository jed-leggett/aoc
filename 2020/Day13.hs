{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (catMaybes,fromMaybe, isJust, isNothing)
import Data.List (maximumBy, sortOn)
import Data.List.Split (splitOn)
import Data.Text (unpack)

import AOC

data BusId =
  BusId
  { index    :: Int
  , busId    :: Int
  , multiple :: Int
  }
  deriving Show

mainA :: IO ()
mainA = do
  ls <- fmap (map unpack) (readFileTextLines "Day13Input")
  let departure = (read $ parseString (manyTill digit (try eof)) $ head ls) :: Int
  let busIds = (parseString parseBusId) <$> (splitOn "," $ last ls)
  let earliestDepartures = (findEarliestDepartureTime departure 0) <$> catMaybes busIds
  let sortedDepatures = sortOn snd earliestDepartures
  print $ (\ (i,d) -> i*(d-departure)) (head sortedDepatures)

-- mainB' :: IO ()
-- mainB' = do
--   ls <- fmap (map unpack) (readFileTextLines "Day13Input")
--   let busIds = (parseString parseBusId) <$> (splitOn "," $ last ls)
--   let indexedBusIds = zip [0 .. length busIds -1] busIds
--   let busIds' = mkBusId <$> indexedBusIds
--   let firstLast = getNextFirstLast busIds'
--   let b = getNextFirstLast firstLast
--   let c = getNextFirstLast b
--   let d = getNextFirstLast c
--   meh <- findConsecutiveDepartures busIds'
--   -- mapM_ print [firstLast,b,c,d]
--   print meh
--   -- mapM_ print $ findConsecutiveDepartures busIds'

mainB :: IO ()
mainB = do
  ls <- fmap (map unpack) (readFileTextLines "Day13Input")
  let busIds = (parseString parseBusId) <$> (splitOn "," $ last ls)
  let indexedBusIds = (\(idx, Just i) -> (idx,i)) <$> ((filter (isJust . snd)) $ zip [0 .. length busIds -1] busIds)
  let skip = maximumBy (\a b ->compare (snd a) (snd b)) indexedBusIds
  meh <- foo skip 0 indexedBusIds
  print meh

foo :: (Int,Int) -> Int -> [(Int, Int)] -> IO Int
foo skip@(offset,skipVal) i ls = do
  -- print i
  if 0 == (sum $ (\(idx,busId) -> mod (idx+i-offset) busId) <$> ls)
    then pure i
    else foo skip (i+skipVal) ls

-- mkBusId :: (Int, Maybe Int) -> BusId
-- mkBusId (idx, mi) =
--   BusId idx (fromMaybe 1 mi) (fromMaybe 1 mi)

-- getNextFirstLast :: [BusId] -> [BusId]
-- getNextFirstLast (BusId fidx fid fm:ls) =
--   getNextFirstLast' $ (BusId fidx fid $ fid+fm):ls

-- getNextFirstLast' :: [BusId] -> [BusId]
-- getNextFirstLast' ls = do
--   let l = length ls -1
--   if fm+l == lm
--     then ls
--     else
--       if fm+l > lm
--         then getNextFirstLast' (newList ls (fElm,newElm lElm))
--         else getNextFirstLast' (newList ls (newElm fElm,lElm))
--   where
--     fElm@(BusId fidx fid fm) = head ls
--     lElm@(BusId lidx lid lm) = last ls
--     newElm a@(BusId idx id' m) = a { multiple = m + id'}
--     newList :: [BusId] -> (BusId,BusId) -> [BusId]
--     newList ls (a,b) = do
--       let
--         middleOfList = reverse . tail . reverse . tail $ ls
--       a:middleOfList ++ [b]

-- findConsecutiveDepartures :: [BusId] -> IO [BusId]
-- findConsecutiveDepartures ls@(f:_) = do
--   print ls
--   let maybeAnswer = multiplyBusId f <$> ls
--   if 0 /= (length $ filter isNothing maybeAnswer)
--     then findConsecutiveDepartures (getNextFirstLast ls)
--     else pure $ catMaybes maybeAnswer

-- multiplyBusId :: BusId -> BusId -> Maybe BusId
-- multiplyBusId f@(BusId _ _ t) x@(BusId idx id' m) =
--   if t+idx == m 
--     then Just x
--     else if t+idx < m
--       then Nothing
--       else multiplyBusId f (BusId idx id' (m+id'))


findEarliestDepartureTime :: Int -> Int -> Int -> (Int,Int)
findEarliestDepartureTime departureTime busIdMulti busId =
  if busIdMulti >= departureTime
    then (busId, busIdMulti)
    else findEarliestDepartureTime departureTime (busIdMulti+busId) busId

parseBusId :: Parser (Maybe Int)
parseBusId = (fmap. fmap) read $ optionMaybe (manyTill digit (try eof))
