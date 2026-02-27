#!/usr/bin/env runhaskell

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
  . lines
  =<< getContents

part2 :: IO ()
part2
  = print
  . lines
  =<< getContents
