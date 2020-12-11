module Advent.Day5 where

import Advent.Prelude
import Data.Attoparsec.Text
import Data.Bits
import Data.Word

import Data.List (sort, head) 

part1 :: IO ()
part1 = print . maximum . fmap (uncurry seatId) . either error id . parseOnly (ticket `sepBy` endOfLine) =<< getContents

part2 :: IO ()
part2 = print . missingSeat . fmap (uncurry seatId) . either error id . parseOnly (ticket `sepBy` endOfLine) =<< getContents

missingSeat :: [Int] -> Int
missingSeat = head . mapMaybe (uncurry skipped) . pairs . sort
 where
  skipped before after = do
    guard $ after - before == 2
    pure $ before + 1

pairs :: [a] -> [(a, a)]
pairs xs = zip xs $ drop 1 xs

seatId :: Word8 -> Word8 -> Int
seatId r c = (fromIntegral r * 8) + fromIntegral c

ticket :: Parser (Word8, Word8)
ticket = (,) <$> row <*> column

row :: Parser Word8
row = flipBits <$> row'

row' :: Bits a => Parser [a -> Int -> a]
row' = count 7 $ choice
    [ clearBit <$ char 'F'
    , setBit <$ char 'B'
    ]

column :: Parser Word8
column = flipBits <$> column'

column' :: Bits a => Parser [a -> Int -> a]
column' = count 3 $ choice
    [ clearBit <$ char 'L'
    , setBit <$ char 'R'
    ]

flipBits :: [Word8 -> Int -> Word8] -> Word8
flipBits = foldr (\(i, f) acc-> f acc i) 0 . zip [0..] . reverse