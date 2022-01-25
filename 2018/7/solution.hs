#!/usr/bin/env stack
-- stack --resolver lts-18.13 script

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.Char                    as Char
import           Data.List                    ((\\))
import qualified Data.List                    as L
import qualified Data.Map.Strict              as Map
import qualified Data.Maybe                   as Maybe
import qualified Data.Set                     as Set
import           System.Environment           (getArgs)
import qualified Text.ParserCombinators.ReadP as Parse
import qualified Text.Read                    as Read

import qualified Debug.Trace                  as TR

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


-- in order to run example from README:
-- 1. set reduceMapWithWorkers 2 cost1 (0,"", [])
-- 2. cat example.txt | stack solution.hs "part2"
-- (15,"CABFDE")
part2 :: IO ()
part2
    = print
    . reduceMapWithWorkers 5 cost (0,"", [])
    . enrichMap
    . foldr (\(Relation prereq letter) -> Map.insertWith (<>) letter (Set.singleton prereq)) Map.empty
    . Maybe.mapMaybe (Read.readMaybe @Relation)
    . lines
    =<< getContents

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
    let letterToRemove = case filter (\(_,vals) -> Set.null vals) toprocess of
            l:_ -> fst l
            []  -> error "letterToRemove should not be empty"
        toprocess' = filter (\(l,_) -> letterToRemove /= l) toprocess
        toprocess'' = map (\(l,vals) -> (l, vals `Set.difference` Set.singleton letterToRemove)) toprocess'
    in reduceMap (letterToRemove : res) toprocess''

-- A=1, B=2, ..
cost1 :: Char -> Int
cost1 c = fromEnum c - 64

-- A=60+1, B=60+2, ..
cost :: Char -> Int
cost c = fromEnum c - 4

--workerN number of workers available
--costF cost of letter processing
--res is (currentCost, letters processed and inverted, letter being processed expressed as (letter, cost incurred at the moment))
reduceMapWithWorkers workerN costF (c, res, _) []        = (c, reverse res)
reduceMapWithWorkers workerN costF (currentCost,res,lettersProcessed) toprocess =
        -- all potential letters with no prerequisite that could be processed
    let potentialLettersToRemove = case filter (\(_,vals) -> Set.null vals) toprocess of
            [] -> error "potentialLettersToRemove should not be empty"
            l  -> map fst l

        -- all potential letters with no prerequisite that have not been started processing before
        freeLettersToRemove = potentialLettersToRemove \\ (map fst lettersProcessed)

        -- letters that start to process (there are free workers to realize that)
        lettersStartingToProcess = take (workerN - length lettersProcessed) freeLettersToRemove

        -- letters starting with cost
        lettersStartingToProcessWithCost = map (\l -> (l, costF l)) lettersStartingToProcess

        -- all letters taken into consideration in this round
        allLettersToConsider = lettersProcessed ++ lettersStartingToProcessWithCost

        -- letter that is to be removed in this round
        (letterToRemove,costRemaining):restLetters = L.sortOn snd allLettersToConsider

        -- as we are processing in parallel we are substracting the cost of the chosen letter in all other processed letters
        restLetters' = map (\(l,c) -> (l, c - costRemaining) ) restLetters
        toprocess' = filter (\(l,_) -> l /= letterToRemove) toprocess
        toprocess'' = map (\(l,vals) -> (l, vals `Set.difference` Set.singleton letterToRemove)) toprocess'
    in reduceMapWithWorkers workerN costF (currentCost + costRemaining, letterToRemove : res, restLetters') toprocess''
