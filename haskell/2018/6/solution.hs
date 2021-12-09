#!/usr/bin/env stack
-- stack --resolver lts-18.13 script

{-# LANGUAGE TypeApplications #-}

import           Control.Arrow                ((&&&))
import qualified Data.Char                    as Char
import qualified Data.List                    as L
import qualified Data.Map.Strict              as Map
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
    . retrieveCoordinateWithMostPoints
    . filterOutInfinites
    . coalesceCoords
    . (\(boundary, surface, coords) -> (boundary, map (calculateClosestCoord coords) surface))
    . getBoundaries
    . Maybe.mapMaybe (Read.readMaybe @Coordinate)
    . lines
    =<< getContents

data Coordinate
  = Coordinate (Word, Word)
  deriving (Ord, Show, Eq)

parserNum :: Parse.ReadP Word
parserNum = fmap read (Parse.munch1 Char.isDigit)

parserCoordinate :: Parse.ReadP Coordinate
parserCoordinate = do
    x <- parserNum
    Parse.string ", "
    y <- parserNum
    pure $ Coordinate (x, y)

instance Read Coordinate where
  readsPrec _ = Parse.readP_to_S parserCoordinate

getSurface :: (Word, Word) -> [(Word,Word)]
getSurface (width, height) =
  (,) <$> [ 0 .. width + 1 ] <*> [ 0 .. height + 1 ]

getBoundaries coords =
  (\pair -> (pair, getSurface pair,coords)) $
  uncurry (,) $
  (optimum fst &&& optimum snd) coords
  where
    optimum f = maximum . map (\(Coordinate pair) -> f pair)

calculateClosestCoord coords (x,y) =
    analyzeClosest $ L.sortOn fst $ map dist coords
  where
    abs' x y =
      if x >= y then x - y
      else y - x
    dist (Coordinate coord@(cx, cy)) = (abs' x cx + abs' y cy, coord)
    analyzeClosest ((_d,coord):[]) = ((x,y), Just coord)
    analyzeClosest a@((d1,coord):(d2,_):_) =
      if d1 == d2 then ((x,y),Nothing)
      else ((x,y), Just coord)

coalesceCoords (pair, rest) =
  (pair, coalesce rest)
  where
    project (point, Nothing)    = Nothing
    project (point, Just coord) = Just (point, coord)
    coalesce =
      foldr (\(point, coord) -> Map.insertWith (++) coord [point]) Map.empty
      . Maybe.mapMaybe project

filterOutInfinites ((xMax,yMax), rest) =
  Map.filter (\points -> all xIsZero points && all yIsZero points) rest
  where
    xIsZero (x,_) = x /= 0 && x /= xMax
    yIsZero (_,y) = y /= 0 && y /= yMax

retrieveCoordinateWithMostPoints =
    L.take 1
  . reverse
  . L.sortOn snd
  . Map.toList
  . Map.map length
