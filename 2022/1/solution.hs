#!/usr/bin/env stack
-- stack --resolver lts-18.13 script

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}


import qualified Data.Char                    as Char
import qualified Data.List                    as L
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
part2
    = print
    . fmap (sum . L.take 3 . L.reverse . L.sort . calCalories)
    . Read.readMaybe @ItemList
    =<< getContents


part1 :: IO ()
part1
    = print
    . fmap (maximum . calCalories)
    . Read.readMaybe @ItemList
    =<< getContents

calCalories :: ItemList -> [Word]
calCalories (ItemList items) =
    calculate <$> items
 where
    calculate (Item ss) = sum ss

data ItemList = ItemList [Item]
    deriving Show

data Item = Item [Word]
    deriving Show

parserBlankline :: Parse.ReadP ()
parserBlankline = Parse.choice
    [ Parse.char '\n' >> return ()
    , Parse.eof
    ]

parserNum :: Parse.ReadP Word
parserNum = fmap read (Parse.munch1 Char.isDigit)

parserItem :: Parse.ReadP Item
parserItem  = do
    item <- Parse.sepBy1 parserNum parserBlankline
    parserBlankline
    pure $ Item item

parserItemList :: Parse.ReadP ItemList
parserItemList =
    ItemList <$> Parse.sepBy1 parserItem parserBlankline

instance Read ItemList where
  readsPrec _ = Parse.readP_to_S parserItemList

instance Read Item where
  readsPrec _ = Parse.readP_to_S parserItem
