#!/usr/bin/env stack
{- stack script
   --resolver lts-22.37
   --package "containers HUnit"
   --ghc-options -Wall
-}

{-# LANGUAGE TypeApplications #-}

import qualified Data.Char                    as Char
import qualified Data.Map.Strict              as Map
import qualified Data.Maybe                   as Maybe
import           System.Environment           (getArgs)
import qualified System.Exit                  as Exit
import           Test.HUnit                   (Test (..), failures, runTestTT,
                                               (@?=))
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
      else if arg == "tests" then
          testing
      else
          print "Waiting for 'part1', 'part2' or 'tests'"
    _       -> print "Waiting for one command argument either 'part1', 'part2' or 'tests'"

part2 :: IO ()
part2 =
      print
    . part2Logic
    . Maybe.mapMaybe (Read.readMaybe @Something)
    . lines
    =<< getContents

part2Logic :: [Something] -> Word
part2Logic = undefined

part1 :: IO ()
part1 =
      print
    . part1Logic
    . Maybe.mapMaybe (Read.readMaybe @Something)
    . lines
    =<< getContents

part1Logic :: [Something] -> Word
part1Logic = undefined

testing :: IO ()
testing = do
    result <- runTestTT testSuite
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
 where
    testSuite = TestList
        [ TestLabel "1 is 1" testOne
        ]
    testOne = TestCase $ (1::Word) @?= (1::Word)

data Something =
    Something {unSomething :: Map.Map String Word} deriving (Eq, Show)

parserNum :: Parse.ReadP Word
parserNum = fmap read (Parse.munch1 Char.isDigit)

parserSomething :: Parse.ReadP Something
parserSomething = do
    Parse.skipSpaces
    num <- parserNum
    pure $ Something $ Map.fromList [("something", num)]

instance Read Something where
  readsPrec _ = Parse.readP_to_S parserSomething
