module Advent.Day3 where

import Advent.Prelude
import Data.Attoparsec.Text
    (many1, sepBy, char, endOfLine, parseOnly, Parser)
import Data.List (tail, unfoldr)
import Safe (headMay, tailMay)
import Control.Arrow ((>>>))

part1 :: IO ()
part1 = do
    position <- either error toPosition . parseOnly parseMap <$> getContents
    let keys = search (right >>> right >>> right >>> down) position
    print . length $ filter (== Tree) keys

part2 :: IO ()
part2 = do 
    position <- either error toPosition . parseOnly parseMap <$> getContents
    let results = ($ position) . search <$> paths
    print . product $ length . filter (== Tree) <$> results
  where
    paths =
        [ right >>> down
        , right >>> right >>> right >>> down
        , right >>> right >>> right >>> right >>> right >>> down
        , right >>> right >>> right >>> right >>> right >>> right >>> right >>> down
        , right >>> down >=> down
        ]

data Key = Tree | Open
    deriving stock (Show, Eq)

parseMap :: Parser [[Key]]
parseMap = parseRow `sepBy` endOfLine 

parseRow :: Parser [Key]
parseRow = many1 parseKey 

parseKey :: Parser Key
parseKey = open <|> tree
    where
        open = Open <$ char '.'
        tree = Tree <$ char '#'

search :: (Position a -> Maybe (Position a)) -> Position a -> [a]
search move = unfoldr go
    where
        go x =
            let next = move x
            in (,) <$> (position <$> next) <*> next

data Position a = Position 
    { position :: a
    , right :: Position a
    , down :: Maybe (Position a)
    }

toPosition :: [[a]] -> Position a
toPosition [] = error "evil partial function"
toPosition (row:rest) = toPosition' (cycle row) (cycle <$> rest)

toPosition' :: [a] -> [[a]] -> Position a
toPosition' [] _rest = error "evil partial function"
toPosition' (position:row) rest = Position
    { position
    , right = toPosition' row (fmap tail rest)
    , down = toPosition' <$> headMay rest <*> tailMay rest
    }