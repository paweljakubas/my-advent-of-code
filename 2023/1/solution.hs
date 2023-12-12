#!/usr/bin/env stack
-- stack --resolver lts-18.13 script

{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Arrow                ((&&&))
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
    . sum
    . map extractTwoDigitsNum
    . Maybe.mapMaybe (Read.readMaybe @DigitsInLine)
    . lines
    =<< getContents

data DigitsInLine
  = DigitsInLine { digits :: [Word] }
  deriving Show

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
