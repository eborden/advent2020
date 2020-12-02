module Advent.Main
  ( main
  )
where

import Advent.Prelude
import qualified Advent.Day1 as Day1

main :: IO ()
main = do
  day <- parseDay
  part <- parsePart
  case day of
    1 -> case part of
      Part1 -> Day1.part1
      Part2 -> Day1.part2
    _
      | day <= 25 -> usage "Day not implemented yet"
      | otherwise -> usage "Day out of range"

parseDay :: IO Natural
parseDay = do
  day <- expect "Must specify day" =<< lookupEnv "DAY"
  expect "Could not parse day" $ readMaybe day

parsePart :: IO Part
parsePart = do
  mPart <- lookupEnv "PART"
  case mPart of
    Nothing -> pure Part1
    Just part -> expect "Could not parse part" $ do
      n <- readMaybe @Natural part
      Part1 <$ guard (n == 1) <|> Part2 <$ guard (n == 2)

usage :: Text -> IO a
usage message = do
  name <- pack <$> getProgName
  die $ unpack $ unlines
    [ message
    , ""
    , "usage: DAY=N [PART=N] " <> name
    , "  where 1 <= N <= 25"
    , "  where 1 <= P <= 2"
    ]

expect :: Text -> Maybe a -> IO a
expect message = maybe (usage message) pure
