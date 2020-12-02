{-# LANGUAGE ViewPatterns #-}

module Advent.Day2 where

import Advent.Prelude hiding (takeWhile)
import Data.Bits (xor)
import Data.Text (index)
import qualified Data.Text as Text
import Data.Attoparsec.Text
    ( sepBy
    , decimal
    , isEndOfLine
    , isHorizontalSpace
    , skipSpace
    , anyChar
    , char
    , endOfLine
    , parseOnly
    , takeTill
    , Parser
    )

part1 :: IO ()
part1 = print =<< either error (pure . length . mapMaybe rentalValidate) =<< readEntries

part2 :: IO ()
part2 = print =<< either error (pure . length . mapMaybe tobogganValidate) =<< readEntries

rentalValidate :: Entry -> Maybe Entry
rentalValidate entry@Entry{policy = Policy{range, letter}, password}
    | numChars `between` range = Just entry
    | otherwise = Nothing
 where
  numChars = length . filter (== letter) $ unpack password
  between i (low, high) = low <= i && i <= high

tobogganValidate :: Entry -> Maybe Entry
tobogganValidate entry@Entry{policy = Policy{range = (pred -> i, pred -> j), letter}, password}
    | matchIndicies = Just entry
    | otherwise = Nothing
 where
  len = Text.length password
  matchIndicies 
    | len <= i || len <= j = False
    | otherwise = xor (index password i == letter) (index password j == letter)

data Entry = Entry
    { policy :: Policy
    , password :: Text
    }
    deriving stock Show

data Policy = Policy
    { range :: (Int, Int)
    , letter :: Char
    }
    deriving stock Show

readEntries :: IO (Either String [Entry])
readEntries =
    parseOnly (parseEntry `sepBy` endOfLine) <$> getContents

parseEntry :: Parser Entry
parseEntry = Entry
    <$> parsePolicy
    <*> (skipSpace *> takeTill (\x -> isEndOfLine x || isHorizontalSpace x))

parsePolicy :: Parser Policy
parsePolicy = Policy
    <$> parseRange <* skipSpace
    <*> anyChar <* char ':'

parseRange :: Parser (Int, Int)
parseRange = (,)
    <$> decimal
    <*> (char '-' *> decimal)