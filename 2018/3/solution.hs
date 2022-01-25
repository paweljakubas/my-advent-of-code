#!/usr/bin/env stack
-- stack --resolver lts-18.13 script

{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Arrow                ((&&&))
import qualified Data.Char                    as Char
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

part1 :: IO ()
part1
    = print
    . Map.size
    . Map.filter (>1)
    . foldr (\ ( x, y ) -> Map.insertWith (+) ( x, y ) 1) Map.empty
    . concatMap getSurface
    . Maybe.mapMaybe (Read.readMaybe @Claim)
    . lines
    =<< getContents

part2 :: IO ()
part2
    = print
    . Map.keys
    . filterClaims
    . (createSurfaceMap &&& calculateOverlapping)
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

-------
--xxx--
--xxx--
-------
--we count x from left to right starting from 1
--we count y from top to bottom starting from 1
--x -> 3,4,5
--y -> 2,3
getSurface :: Claim -> [(Word,Word)]
getSurface (Claim _ left top width height) =
  (,) <$> [ left .. left + width - 1 ] <*> [ top .. top + height - 1 ]

calculateOverlapping :: [Claim] -> Map.Map (Word, Word) Word
calculateOverlapping =
    foldr (\ ( x, y ) -> Map.insertWith (+) ( x, y ) 1) Map.empty
  . concatMap getSurface

createSurfaceMap :: [Claim] -> Map.Map Word [(Word, Word)]
createSurfaceMap =
  Map.fromList . map processClaim
  where
    processClaim claim@(Claim theid _ _ _ _) =
      (theid, getSurface claim)

filterClaims :: (Map.Map Word [(Word, Word)], Map.Map (Word, Word) Word) -> Map.Map Word [(Word, Word)]
filterClaims (claims, surface) =
  Map.filter isSurfaceNotShared claims
  where
    countPred = \case
      Just v -> v <= 1
      Nothing -> False
    isSurfaceNotShared =
      all (\point -> countPred (Map.lookup point surface))
