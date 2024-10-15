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
    . Maybe.mapMaybe (Read.readMaybe @Game)
    . lines
    =<< getContents

part2Logic :: [Game] -> Word
part2Logic =
      sum
    . map (productCube . coalesceGame)

productCube :: GameCoalesed -> Word
productCube (GameCoalesed _ (Cube themap)) =
    product $ Map.elems themap

coalesceGame :: Game -> GameCoalesed
coalesceGame (Game num cubes) =
    let aggr = foldr (Map.unionWith max . unCube) Map.empty cubes
    in GameCoalesed num (Cube aggr)

part1 :: IO ()
part1 =
      print
    . part1Logic
    . Maybe.mapMaybe (Read.readMaybe @Game)
    . lines
    =<< getContents

part1Logic :: [Game] -> Word
part1Logic =
      sum
    . map num
    . filter (\(Game _ games) -> foldr (\game res -> res && checkGame game) True games)
 where
   limit = [("blue", 14),("red", 12),("green", 13)]
   within (key, threshold) (Cube themap) = case Map.lookup key themap of
       Nothing  -> True
       Just val -> val <= threshold
   checkGame cube = foldr (\pair res -> res && within pair cube) True limit

testing :: IO ()
testing = do
    result <- runTestTT testSuite
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
 where
    testSuite = TestList
        [ TestLabel "can read red" testRed
        , TestLabel "can read blue" testBlue
        , TestLabel "can read green" testGreen
        , TestLabel "can read game" testGame1
        , TestLabel "part1 logic for sample is fine" testLogic1
        , TestLabel "part2 logic for sample is fine" testLogic2
        ]
    testRed = TestCase $ (read @Cube " 10 red") @?= (Cube $ Map.fromList [("red", 10)])
    testBlue = TestCase $ (read @Cube " 11 blue") @?= (Cube $ Map.fromList [("blue", 11)])
    testGreen = TestCase $ (read @Cube "1210 green") @?= (Cube $ Map.fromList [("green", 1210)])
    testGame1 = TestCase $
        (read @Game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
        @?=
        (Game 1
         [ Cube $ Map.fromList [("blue", 3),("red", 4)]
         , Cube $ Map.fromList [("red", 1),("green", 2),("blue", 6)]
         , Cube $ Map.fromList [("green", 2)]]
        )
    sample = [
          Game 1
         [ Cube $ Map.fromList [("blue", 3),("red", 4)]
         , Cube $ Map.fromList [("red", 1),("green", 2),("blue", 6)]
         , Cube $ Map.fromList [("green", 2)]
         ]
        , Game 2
         [ Cube $ Map.fromList [("blue", 1),("green", 2)]
         , Cube $ Map.fromList [("red", 1),("green", 3),("blue", 4)]
         , Cube $ Map.fromList [("green", 1),("blue", 1)]
         ]
        , Game 3
         [ Cube $ Map.fromList [("blue", 6),("green", 8),("red", 20)]
         , Cube $ Map.fromList [("red", 4),("green", 13),("blue", 5)]
         , Cube $ Map.fromList [("green", 5),("red", 1)]
         ]
        , Game 4
         [ Cube $ Map.fromList [("blue", 6),("green", 1),("red", 3)]
         , Cube $ Map.fromList [("red", 6),("green", 3)]
         , Cube $ Map.fromList [("green", 3),("red", 14),("blue", 15)]
         ]
        , Game 5
         [ Cube $ Map.fromList [("blue", 1),("green", 3),("red", 6)]
         , Cube $ Map.fromList [("green", 2),("red", 1),("blue", 2)]
         ]
        ]
    testLogic1 = TestCase $ (part1Logic sample) @?= 8
    testLogic2 = TestCase $ (part2Logic sample) @?= 2286

data Cube = Cube {unCube :: Map.Map String Word} deriving (Eq, Show)

parserNum :: Parse.ReadP Word
parserNum = fmap read (Parse.munch1 Char.isDigit)

parserClaim :: Parse.ReadP Cube
parserClaim = do
    Parse.skipSpaces
    colorNumPairs <- Parse.sepBy1 validColorParser (Parse.string ",")
    pure $ Cube $ foldr (uncurry Map.insert) Map.empty colorNumPairs
 where
   colorParser str = do
       Parse.skipSpaces
       num <- parserNum
       Parse.skipSpaces
       _ <- Parse.string str
       pure (str, num)
   validColorParser =
       Parse.choice [colorParser "blue", colorParser "red", colorParser "green"]

instance Read Cube where
  readsPrec _ = Parse.readP_to_S parserClaim

data Game = Game {num :: Word, cubes :: [Cube]} deriving (Eq, Show)

parserGame :: Parse.ReadP Game
parserGame = do
    Parse.skipSpaces
    _ <- Parse.string "Game"
    Parse.skipSpaces
    num <- parserNum
    _ <- Parse.string ":"
    Parse.skipSpaces
    cubes <- Parse.sepBy1 parserClaim (Parse.string ";")
    pure $ Game num cubes

instance Read Game where
  readsPrec _ = Parse.readP_to_S parserGame

data GameCoalesed = GameCoalesed {theid :: Word, cube :: Cube} deriving (Eq, Show)
