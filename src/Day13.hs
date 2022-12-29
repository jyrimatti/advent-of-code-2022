{-# LANGUAGE OverloadedStrings #-}
module Day13 where
import Universum
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.List.Extra (split)
import Data.Foldable.Extra (sumOn')
import Text.Megaparsec (Parsec, parseMaybe, between, sepBy)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Applicative.Combinators (eitherP)
import Data.List.Predicate (sorted)

input :: IO [[Text]]
input = split (== "") . lines <$> readFile "input/input13.txt"

type Parser = Parsec () Text

parse :: Parser a -> Text -> a
parse = fromJust ... parseMaybe

data PacketData = List [PacketData] | Value Int
  deriving Eq

instance Ord PacketData where
    Value x <= Value y = x <= y
    List x  <= List y  = x <= y
    Value x <= List y  = List [Value x] <= List y
    List x  <= Value y = List x <= List [Value y]

packetP :: Parser PacketData
packetP = either List Value <$> eitherP (between "[" "]" $ sepBy packetP ",") decimal

solution1 :: IO Int
solution1 = sumOn' fst . filter snd . zip [1..] . fmap (sorted . fmap (parse packetP)) <$> input
-- 5390

divider1, divider2 :: PacketData
divider1 = parse packetP "[[2]]"
divider2 = parse packetP "[[6]]"

search :: PacketData -> [PacketData] -> Int
search = succ . fromJust ... elemIndex

solution2 :: IO Int
solution2 = ((*) <$> search divider1 <*> search divider2) . sort . ([divider1,divider2] <>) . fmap (parse packetP) . concat <$> input
-- 19261