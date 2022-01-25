#!/usr/bin/env stack
-- stack --resolver lts-18.13 script

{-# LANGUAGE TypeApplications #-}

import qualified Data.Char                    as Char
import qualified Data.List                    as L
import qualified Data.Map.Strict              as Map
import qualified Data.Maybe                   as Maybe
import qualified Data.Time                    as Time
import           System.Environment           (getArgs)
import qualified Text.ParserCombinators.ReadP as Parse
import qualified Text.Read                    as Read

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] ->
      if arg == "part1" then
        part1
      else if arg == "part2" then
        part2
      else
        print "Waiting for 'part1' or 'part2'"
    _       -> print "Waiting for one command argument either 'part1' or 'part2'"

part1 :: IO ()
part1
    = print
    . (\(guardid,_,asleeps) -> (guardid *) <$> biggestOverlapping asleeps)
    . head
    . reverse
    . L.sortOn (\(_,n,_) -> n)
    . map toNumberOfElems
    . sharedPart
    =<< getContents

sharedPart :: String -> [(Word, [[Word]])]
sharedPart
    = Map.toList
    . foldr (\(GuardSession (guardid, asleep) ) -> Map.insertWith (++) guardid [asleep]) Map.empty
    . snd
    . foldl toGuardSession (Nothing, [])
    . L.sortOn (fst . unGuardLog)
    . Maybe.mapMaybe (Read.readMaybe @GuardLog)
    . lines

part2 :: IO ()
part2
    = print
    . fst
    . head
    . reverse
    . L.sortOn snd
    . map (\(guardid, overlapJ) -> (guardid, Maybe.fromJust overlapJ))
    . filter (Maybe.isJust . snd)
    . map (\(guardid, asleeps)-> (guardid, biggestOverlapping asleeps))
    . sharedPart
    =<< getContents

data GuardAction = StartWork Word | FallAsleep | WakeUp
  deriving Show

newtype GuardLog
  = GuardLog { unGuardLog :: (Time.UTCTime, GuardAction) }
  deriving Show

newtype GuardSession
  = GuardSession { unGuardSession :: (Word, [Word])}
  deriving Show
   -- guard number and minutes between 00-59 that they were asleep

biggestOverlapping
  :: [[Word]]
  -> Maybe Word
biggestOverlapping
  = checkIfAnyOverlapping
  . foldr (\x -> Map.insertWith (+) x 1) Map.empty
  . concat
  where
    checkIfAnyOverlapping m =
      if m == Map.empty then
        Nothing
      else
        Just $ fst $ head $ reverse $ L.sortOn snd $ Map.toList m

toNumberOfElems
  :: (Word, [[Word]])
  -> (Word, Int, [[Word]])
toNumberOfElems (guardid, lists) =
  (guardid, sum (length <$> lists), lists)

toGuardSession
  :: (Maybe Word, [GuardSession])
  -> GuardLog
  -> (Maybe Word, [GuardSession])
toGuardSession (asleepM, sessions) (GuardLog (time, action)) =
  let Time.TimeOfDay _ minutes _ = Time.timeToTimeOfDay (Time.utctDayTime time)
  in case action of
    StartWork guardid -> (Nothing, GuardSession (guardid, []):sessions )
    FallAsleep        -> (Just $ fromIntegral minutes, sessions)
    WakeUp          ->
      let GuardSession (guardid, asleeps) = head sessions
          closedSessions = tail sessions
          (Just startAsleep) = asleepM
      in  (Nothing, GuardSession (guardid, asleeps ++ [startAsleep .. fromIntegral minutes - 1]):closedSessions)

parserNum :: Parse.ReadP Word
parserNum = fmap read (Parse.munch1 Char.isDigit)

parserTime :: Parse.ReadP Time.UTCTime
parserTime = do
  year <- fromIntegral <$> parserNum
  Parse.string "-"
  month <- fromIntegral <$> parserNum
  Parse.string "-"
  day <- fromIntegral <$> parserNum
  Parse.string " "
  hour <- fromIntegral <$> parserNum
  Parse.string ":"
  minute <- fromIntegral <$> parserNum
  pure $ Time.UTCTime (Time.fromGregorian year month day) (Time.timeOfDayToTime (Time.TimeOfDay hour minute 0))

parserGuardAction :: Parse.ReadP GuardLog
parserGuardAction = do
    Parse.string "["
    thetime <- parserTime
    Parse.string "] "
    state <- parseStarting Parse.+++ parseFallingAsleep Parse.+++ parseWakingUp
    pure $ GuardLog (thetime, state)
  where
    parseStarting = do
      Parse.string "Guard #"
      gNum <- parserNum
      Parse.string " begins shift"
      pure $ StartWork gNum
    parseFallingAsleep = do
      Parse.string "falls asleep"
      pure FallAsleep
    parseWakingUp = do
      Parse.string "wakes up"
      pure WakeUp

instance Read GuardLog where
  readsPrec _ = Parse.readP_to_S parserGuardAction
