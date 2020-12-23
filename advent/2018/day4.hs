{-# LANGUAGE OverloadedStrings #-}

module Day4 where

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

main :: IO ()
main = do
   ls <- Text.lines <$> TextIO.readFile "day4input"
   let 
      guardEvents = sortWith dateTime $ parseGuardEvent <$> ls
   sleepHabits <- mapGuardSleepHabits guardEvents Map.empty
   strategy1 sleepHabits
   strategy2 sleepHabits

strategy1 :: Map Int SleepHabits -> IO ()
strategy1 sleepHabits = do
   let (sleepiestId, sleepiestGuard) = head . reverse $ sortWith (totalSleep . snd) (Map.assocs sleepHabits)
   print . show $ ((*) sleepiestId) <$> (elemIndex (head . reverse . sort $ sleepByMinute sleepiestGuard) (sleepByMinute sleepiestGuard))

strategy2 :: Map Int SleepHabits -> IO ()
strategy2 sleepHabits = do
   let (sleepiestId, sleepiestGuard) = head . reverse $ sortWith (head . reverse . sort . sleepByMinute . snd) (Map.assocs sleepHabits)
   print . show $ ((*) sleepiestId) <$> (elemIndex (head . reverse . sort $ sleepByMinute sleepiestGuard) (sleepByMinute sleepiestGuard))

data GuardEvent =
   GuardEvent
   { eventType :: EventType
   , guardId :: Int
   , dateTime :: UTCTime
   } deriving Show

data SleepHabits =
   SleepHabits
   { totalSleep :: Int
   , sleepByMinute :: [Int]
   } deriving Show

data EventType =
     ShiftStart
   | NapStart
   | NapEnd
   deriving Show

parseGuardEvent :: Text -> GuardEvent
parseGuardEvent line =
   let
      (date: (rest:_)) = Text.splitOn "]" (Text.drop 1 line)
      (eventType, guardId) = 
         case Text.unpack rest of
            " falls asleep" -> (NapStart, 0)
            " wakes up" -> (NapEnd, 0)
            shiftStartText ->
               let
                  (_:guardIdText:_) = Text.splitOn " " $ Text.drop 1 $ Text.pack shiftStartText
               in (ShiftStart, (read (drop 1 $ Text.unpack guardIdText) :: Int))
   in
      GuardEvent
         eventType
         guardId
         (read $ Text.unpack date ++ ":00" :: UTCTime)

mapGuardSleepHabits :: [GuardEvent] -> Map Int SleepHabits -> IO (Map Int SleepHabits)
mapGuardSleepHabits [] sleepHabits = pure sleepHabits
mapGuardSleepHabits (quardEvent:events) allSleepHabits = do
   let
      newSleepHabits = SleepHabits 0 (take 60 $ repeat 0)
      sleepHabit = fromMaybe newSleepHabits $ Map.lookup (guardId quardEvent) allSleepHabits
   (updatedSleepHabits, restOfEvents) <- processNaps events sleepHabit
   let newMap = Map.insert (guardId quardEvent) updatedSleepHabits allSleepHabits
   -- print $ show (guardId quardEvent)
   mapGuardSleepHabits restOfEvents newMap

processNaps :: [GuardEvent] -> SleepHabits -> IO (SleepHabits, [GuardEvent])
processNaps events sleepHabit =
   let (maybeNapTimes, restOfEvents) = processNap events
   in case maybeNapTimes of
         Just (sleepMinute, wakeMinute) ->
            let
               newTotalSleep = (totalSleep sleepHabit) + (wakeMinute - sleepMinute)
               modSleepByMinute = (take sleepMinute (repeat 0)) ++ 
                                    (take (wakeMinute - sleepMinute) (repeat 1)) ++ 
                                    (take (60 - wakeMinute) (repeat 0))
               updatedSleepHabits = SleepHabits newTotalSleep (zipWith (+) modSleepByMinute (sleepByMinute sleepHabit))
            in processNaps restOfEvents updatedSleepHabits
         Nothing -> pure (sleepHabit, events)

processNap :: [GuardEvent] -> (Maybe (Int, Int), [GuardEvent])
processNap [] = (Nothing, [])
processNap (first:second:rest) =
   case (eventType first) of
      ShiftStart -> (Nothing, rest)
      _ ->
         let
            (TimeOfDay _ sleepMinute _) = 
                  localTimeOfDay $ utcToLocalTime (hoursToTimeZone 0) (dateTime first)
            (TimeOfDay _ wakeMinute _) = 
                  localTimeOfDay $ utcToLocalTime (hoursToTimeZone 0) (dateTime second)
         in
            (Just (sleepMinute, wakeMinute), rest)
