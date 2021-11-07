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
    . map (length . reducePolymer)
    . lines
    =<< getContents

reducePolymer :: [Char] -> [Char]
reducePolymer str =
  let go = snd $ fromLeftToRight str
  in if length go == length str then
    str
  else
    reducePolymer go
  where
     toAnnihilate ch1 ch2 = abs (fromEnum ch1 - fromEnum ch2) == 32
     coalesce elem (Nothing, acc) = (Just elem, elem:acc)
     coalesce elem (Just elem1, acc) =
       if toAnnihilate elem elem1 then
         (Nothing, tail acc)
       else
         (Just elem, elem:acc)
     fromLeftToRight = foldr coalesce (Nothing, [])

