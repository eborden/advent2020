module Advent.Day1 where

import Advent.Prelude
import Data.List (head)

-- | A brute force solution
-- 
-- Strings all pairs together and runs through them. It attempts to rely on
-- laziness to do less work, but has a large worst case.
--
part1 :: IO ()
part1 = print . product . find2020 . pairs =<< readInts

part2 :: IO ()
part2 = print . product . find2020 . triads =<< readInts

-- Can fail with read and holds everything in memory
readInts :: IO [Int]
readInts = fmap (read @Int . unpack) . lines . pack <$> getContents

find2020 :: [[Int]] -> [Int]
find2020 =  head . dropWhile ((/= 2020) . sum)

pairs :: Eq a => [a] -> [[a]]
pairs xs = do
    x <- xs
    y <- drop 1 xs
    guard $ x /= y
    pure [x, y]

triads :: Eq a => [a] -> [[a]]
triads xs = do
    x <- xs
    y <- drop 1 xs
    z <- drop 2 xs
    guard $ x /= y && y /= z && x /= z
    pure [x, y, z]