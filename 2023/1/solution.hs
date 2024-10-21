#!/usr/bin/env stack
{- stack script --resolver lts-22.21
   --resolver lts-22.37
   --package "text"
   --ghc-options -Wall
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import           Control.Arrow                ((&&&))
import qualified Data.Char                    as Char
import qualified Data.Maybe                   as Maybe
import qualified Data.Text                    as T
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
part2
    = print
    . sum
    . map extractTwoDigitsNum
    . Maybe.mapMaybe (Read.readMaybe @DigitsInLine)
    . map replaceLine
    . lines
    =<< getContents

part1 :: IO ()
part1
    = print
    . sum
    . map extractTwoDigitsNum
    . Maybe.mapMaybe (Read.readMaybe @DigitsInLine)
    . lines
    =<< getContents

data DigitsInLine
  = DigitsInLine { digits :: [Word] }
  deriving Show

replaceVector :: [(T.Text, T.Text)]
replaceVector =
    [ ("one", "o1e")
    , ("two", "t2o")
    , ("three", "t3e")
    , ("four", "f4r")
    , ("five", "f5e")
    , ("six", "s6x")
    , ("seven", "s7n")
    , ("eight", "e8t")
    , ("nine", "n9e")
    ]

replaceLine :: String -> String
replaceLine =
    T.unpack
    . replaceOneByOne
    . T.pack
  where
    replaceOneByOne txt =
        foldr (\(from, to) txt' -> T.replace from to txt') txt replaceVector

extractTwoDigitsNum :: DigitsInLine -> Word
extractTwoDigitsNum =
      (\(a,b) -> 10*a + b)
    . handlePair
    . ((take 1) &&& (take 1 . reverse))
    . digits
 where
   handlePair = \case
       ([x],[y]) -> (x,y)
       _ -> (0,0)

parserNum :: Parse.ReadP Word
parserNum = fmap (read . singleton) (Parse.satisfy Char.isDigit)
  where
    singleton x = [x]

parserLetter :: Parse.ReadP Char
parserLetter = Parse.satisfy Char.isLetter

parserDigitsInLine :: Parse.ReadP DigitsInLine
parserDigitsInLine = do
    Parse.skipMany parserLetter
    digits <- Parse.sepBy1 parserNum (Parse.many parserLetter)
    Parse.skipMany parserLetter
    pure $ DigitsInLine digits

instance Read DigitsInLine where
  readsPrec _ = Parse.readP_to_S parserDigitsInLine
