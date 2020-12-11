module Advent.Day4 where

import Advent.Prelude hiding (isValid)
import Data.Attoparsec.Text
import qualified Data.Set as Set
import qualified Data.Map as Map

part1 :: IO ()
part1 = print . length . filter isValidFields =<< readRecords

part2 :: IO ()
part2 = print . length . filter isValidTypes =<< readRecords

readRecords :: IO [Record]
readRecords = either error id . parseOnly (recordsWith fields) <$> getContents

isValidFields :: Record -> Bool
isValidFields = Set.null . Set.delete "cid" . Set.difference fieldNames . Map.keysSet

isValidTypes :: Record -> Bool
isValidTypes r =
    "byr" .: yearBetween 1920 2002
    && "iyr" .: yearBetween 2010 2020
    && "eyr" .: yearBetween 2020 2030
    && "hgt" .: (\x -> measure Cm 150 193 x || measure In 59 76 x)
    && "ecl" .: eyeColor
    && "hcl" .: hex
    && "pid" .: pid
  where
    name .: validation = validateField name validation r

yearBetween :: Int -> Int -> Value -> Bool
yearBetween lower upper = \case
    Year n -> lower <= n && n <= upper
    _ -> False

measure :: Measure -> Int -> Int -> Value -> Bool
measure m lower upper = \case
    Measure n m' -> m == m' && lower <= n && n <= upper
    _ -> False
    
eyeColor :: Value -> Bool
eyeColor = \case
    EyeColor _ -> True
    _ -> False
    
hex :: Value -> Bool
hex = \case
    Hex _ -> True
    _ -> False
    
pid :: Value -> Bool
pid = \case
    PID _ -> True
    _ -> False

validateField :: Text -> (Value -> Bool) -> Record -> Bool
validateField name validator = maybe False validator . Map.lookup name

recordsWith :: Parser a -> Parser [a]
recordsWith p = p `sepBy` many1 endOfLine

fields :: Parser Record
fields = fmap Map.fromList $ many1 $ (,) <$> field <*> value <* space
    where field = choice (string <$> toList fieldNames) <* char ':'

fieldNames :: Set Text
fieldNames =  Set.fromList
    [ "byr"
    , "iyr"
    , "eyr"
    , "hgt"
    , "hcl"
    , "ecl"
    , "pid"
    , "cid"
    ]

type Record = Map Text Value

data Value
    = Year Int
    | Measure Int Measure
    | EyeColor Text
    | PID Text
    | Hex Text
    | Whatever Text

data Measure = Cm | In
    deriving stock Eq

value :: Parser Value
value = choice 
    [ char '#' *> (Hex . pack <$> count 6 (digit <|> letter))
    , EyeColor <$> choice ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    , PID . pack <$> count 9 digit <* peekSpace
    , Year . read <$> count 4 digit <* peekSpace
    , Measure <$> decimal <*> ((Cm <$ string "cm") <|> (In <$ string "in"))
    , Whatever <$> takeWhile1 (not . isSpace)
    ]

peekSpace :: Parser ()
peekSpace = do
    x <- peekChar
    maybe mzero (guard . isSpace) x

isSpace :: Char -> Bool
isSpace x = isHorizontalSpace x || isEndOfLine x