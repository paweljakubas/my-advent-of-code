#!/usr/bin/env stack
-- stack --resolver lts-18.13 script

import           Control.Arrow      ((&&&))
import qualified Data.List          as L
import qualified Data.Map           as Map
import qualified Data.Maybe         as Maybe
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
    . uncurry (*)
    . (count 2 &&& count 3)
    . map (Map.elems . foldr (\x -> Map.insertWith (+) x 1) Map.empty)
    . lines
    =<< getContents

count n = length . filter (elem n)

part2 :: IO ()
part2
  = print
  . fmap snd
  . Maybe.listToMaybe
  . filter ((== 1) . fst)
  . map (uncurry ham)
  . pairs
  . lines
  =<< getContents

pairs xs = do
  x : ys <- L.tails xs
  y <- ys
  pure ( x, y )

ham xs ys = foldr
  (\ (x, y) (n, zs) -> if x == y then (n, x : zs) else (n + 1, zs))
  (0, "")
  (zip xs ys)
