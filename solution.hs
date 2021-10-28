#!/usr/bin/env stack
-- stack --resolver lts-18.13 script

{-# LANGUAGE TypeApplications #-}

import qualified Data.Char                    as Char
import qualified Data.Maybe                   as Maybe
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

part2 :: IO ()
part2 = undefined

part1 :: IO ()
part1
    = print
    . Maybe.mapMaybe (Read.readMaybe @Claim)
    . lines
    =<< getContents

data Claim
  = Claim Word Word Word Word Word
  deriving Show

parserNum :: Parse.ReadP Word
parserNum = fmap read (Parse.munch1 Char.isDigit)

parserClaim :: Parse.ReadP Claim
parserClaim = do
    Parse.string "#"
    theid <- parserNum
    Parse.string " @ "
    [left,top] <- Parse.sepBy1 parserNum (Parse.string ",")
    Parse.string ": "
    [width,height] <- Parse.sepBy1 parserNum (Parse.string "x")
    pure $ Claim theid left top width height

instance Read Claim where
  readsPrec _ = Parse.readP_to_S parserClaim
