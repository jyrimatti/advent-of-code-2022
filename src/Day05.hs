{-# LANGUAGE OverloadedStrings #-}
module Day05 where
import Universum hiding (transpose, head, last, init)
import Data.Text (Text, unpack, transpose, head)
import Data.List.Extra (split, init)
import Text.Megaparsec (Parsec, parseMaybe)
import Data.Maybe (fromJust)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (asciiChar, char, letterChar)
import Text.ParserCombinators.ReadP (many1)
import Data.Map (singleton, adjust, (!), insert, delete)
import Data.List (last, (!!))
import Data.Char (isDigit)

input :: IO [Text]
input = lines <$> readFile "input/input05.txt"

toPair :: [b] -> (b, b)
toPair [a,b] = (a,b)

type Parser = Parsec () Text
type StackID = Int
type Stacks = Map StackID [Char]
type Move = (Int,StackID,StackID)

tempStackID :: StackID
tempStackID = -1

parse :: Parser a -> Text -> a
parse = fromJust ... parseMaybe

stackP :: Parser (StackID,[Char])
stackP = (,) <$> decimal <*> many letterChar <* many (char ' ')

moveP :: Parser Move
moveP = (,,) <$> ("move " *> decimal) <*> (" from " *> decimal) <*> (" to " *> decimal)

parseStacks :: [Text] -> Stacks
parseStacks = insert tempStackID [] . fromList . fmap (parse stackP) . filter (isDigit . head) . transpose . reverse

parseMoves :: [Text] -> [Move]
parseMoves = fmap (parse moveP)

apply :: StackID -> StackID -> Stacks -> Stacks
apply from to stacks = adjust init from $ adjust (<> [toRemove]) to stacks
  where toRemove = last $ stacks ! from

applyMove :: Move -> Stacks -> Stacks
applyMove (amount,from,to) = (!! amount) . iterate (apply from to)

parseAll :: [Text] -> (Stacks, [Move])
parseAll = bimap parseStacks parseMoves . toPair . split (== "")

onTop :: Stacks -> [Char]
onTop = fmap last . elems . delete tempStackID

solve :: (Move -> Stacks -> Stacks) -> [Text] -> [Char]
solve f = onTop . uncurry (foldl' $ flip f) . parseAll

solution1 :: IO [Char]
solution1 = solve applyMove <$> input
-- WSFTMRHPP

applyMove9001 :: Move -> Stacks -> Stacks
applyMove9001 (amount,from,to) = applyMove (amount,tempStackID,to) . applyMove (amount,from,tempStackID)

solution2 :: IO [Char]
solution2 = solve applyMove9001 <$> input
-- GSLCMFBRP