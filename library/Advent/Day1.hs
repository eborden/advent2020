module Advent.Day1 (main) where

import Advent.Prelude
import Data.List (tails, head)

-- | A brute force solution
-- 
-- Strings all pairs together and runs through them. It attempts to rely on
-- laziness to do less work, but has a large worst case.
--
main :: IO ()
main = print . uncurry (*) . find2020 . pairs =<< readInts

-- Can fail with read and holds everything in memory
readInts :: IO [Int]
readInts = fmap (read @Int . unpack) . lines . pack <$> getContents

find2020 :: [(Int, Int)] -> (Int, Int)
find2020 =  head . dropWhile ((/= 2020) . uncurry (+))

pairs :: [a] -> [(a, a)]
pairs xs = do
    x <- xs
    ys <- tails xs
    y <- ys
    [(x, y)]