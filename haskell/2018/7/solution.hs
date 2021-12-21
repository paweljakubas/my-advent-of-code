#!/usr/bin/env stack
-- stack --resolver lts-18.13 script

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.Char                    as Char
import qualified Data.List                    as L
import qualified Data.Map.Strict              as Map
import qualified Data.Maybe                   as Maybe
import qualified Data.Set                     as Set
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
    . reduceMap ""
    . enrichMap
    . foldr (\(Relation prereq letter) -> Map.insertWith (<>) letter (Set.singleton prereq)) Map.empty
    . Maybe.mapMaybe (Read.readMaybe @Relation)
    . lines
    =<< getContents

data Relation
  = Relation Char Char
  deriving Show

parserRelation :: Parse.ReadP Relation
parserRelation = do
    Parse.string "Step "
    prerequisite <- Parse.get
    Parse.string " must be finished before step "
    letter <- Parse.get
    Parse.string " can begin."
    pure $ Relation prerequisite letter

instance Read Relation where
  readsPrec _ = Parse.readP_to_S parserRelation

enrichMap m =
    let allVals = foldr Set.union Set.empty $ Map.elems m
        allKeys = Set.fromList $ Map.keys m
      -- Map.toList will produce alphabetically order by key (ie. letter) pairs
    in Map.toList $ foldr (\l -> Map.insert l Set.empty) m (Set.toList $ Set.difference allVals allKeys)

reduceMap res []        = reverse res
reduceMap res toprocess =
    let letterToRemove = case filter (\(_,vals) -> Set.null vals)  toprocess of
            l:_ -> fst l
            []  -> error "letterToRemove should not be empty"
        toprocess' = filter (\(l,_) -> letterToRemove /= l) toprocess
        toprocess'' = map (\(l,vals) -> (l, vals `Set.difference` Set.singleton letterToRemove)) toprocess'
    in reduceMap (letterToRemove : res) toprocess''
