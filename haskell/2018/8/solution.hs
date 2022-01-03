#!/usr/bin/env stack
-- stack --resolver lts-18.13 script

{-# LANGUAGE TypeApplications #-}

import           Control.Monad             (replicateM)
import qualified Control.Monad.Trans.State as State
import           System.Environment        (getArgs)

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
    . metadatas
    . State.evalState getNode
    . map (read @Word)
    . words
    =<< getContents

data Node = Node { children :: [ Node ], metadata :: [ Word ] } deriving (Eq, Show)

getNode = do
  n <- getWord
  m <- getWord
  Node <$> replicateM (fromIntegral n) getNode <*> replicateM (fromIntegral m) getWord

getWord = do
  state <- State.get
  case state of
      first : rest -> do
          State.put rest
          pure first
      _ -> error "expected more numbers to digest"

metadatas node
    = metadata node ++ concatMap metadatas (children node)
