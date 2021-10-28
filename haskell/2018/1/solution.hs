#!/usr/bin/env stack
-- stack --resolver lts-18.13 script

import qualified Data.Set           as Set
import           System.Environment (getArgs)

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
  . sum
  . map read
  . lines
  . filter (/= '+')
  =<< getContents

part2 :: IO ()
part2
  = print
  . f Set.empty
  . scanl (+) 0
  . cycle
  . map read
  . lines
  . filter (/= '+')
  =<< getContents

f s l = case l of
  [] -> Nothing
  h : t -> if Set.member h s
    then Just h
    else f (Set.insert h s) t

