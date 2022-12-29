{-# LANGUAGE OverloadedStrings #-}
module Day14 where
import Universum hiding (maximum, fromList, replicate, set, tail, get, last, head)
import Data.Text (Text)
import Text.Megaparsec (Parsec, parseMaybe, sepBy1)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Maybe (fromJust)
import Data.Foldable (maximum)
import Data.Sequence (replicate, adjust', update, index, (|>))
import Data.List (tail, iterate', last)
import Data.Tuple.Extra (both)
import Algorithm.Search (dfs)

input :: IO [Text]
input = lines <$> readFile "input/input14.txt"

type Parser = Parsec () Text

type Cave = Seq (Seq Char)
type Coord = (Int,Int)

parse :: Parser a -> Text -> a
parse = fromJust ... parseMaybe

newtype Path = Path {p :: [Coord]}

pathP :: Parser Path
pathP = Path <$> sepBy1 (flip (,) <$> decimal <* "," <*> decimal) " -> "

dimensions :: [Path] -> Coord
dimensions = both succ . ((\h w -> (h,w+h)) <$> maximum . fmap fst <*> maximum . fmap snd) . concatMap p

parseCave :: [Path] -> Cave
parseCave = (($) <$> replicate . fst <*> (`replicate` '.') . snd) . dimensions

get :: Coord -> Cave -> Char
get (row,col) = (`index` col) . (`index` row)

set :: Char -> Coord -> Cave -> Cave
set c (row,col) = adjust' (update col c) row

rockify :: Path -> Cave -> Cave
rockify (Path p) m = foldl' (flip $ set '#') m $ concatMap (\((fx,fy),(tx,ty)) -> [(a,b) | a <- [min fx tx..max fx tx], b <- [min fy ty..max fy ty] ]) $ zip p (tail p)

printCave :: Cave -> IO ()
printCave m = traverse_ (putStrLn . toList) $ toList m

start :: (Int, Int)
start = (0,500)

width, height :: Cave -> Int
width = length . (`index` 0)
height = length

neighboring :: Cave -> Coord -> [Coord]
neighboring m (x,y) | x < 0 || y < 0                  = []
neighboring m (x,y) | x >= height m - 1               = []
neighboring m (x,y) | get (x+1,y)   m == '.'          = [(x+1,y)]
neighboring m (x,y) | y > 0 && get (x+1,y-1) m == '.' = [(x+1,y-1)]
neighboring m (x,y) | y >= width m - 1                = [(x+1,y+1)]
neighboring m (x,y) | get (x+1,y+1) m == '.'          = [(x+1,y+1)]
neighboring m (x,y)                                   = []

solution :: Cave -> Coord -> Bool
solution m (x,y) | x >= height m - 1 = False
solution m (x,y) | y >= width m      = True
solution m c                         = null $ neighboring m c

initial :: Cave -> Coord
initial = const start

sandPath :: Cave -> Maybe [Coord]
sandPath = dfs <$> neighboring <*> solution <*> initial

step :: Maybe Cave -> Maybe Cave
step Nothing  = Nothing
step (Just m) = case sandPath m of
    Nothing                            -> Nothing
    Just []                            -> Nothing -- solution == start
    Just cs | snd (last cs) >= width m -> Nothing -- out from right edge
    Just cs                            -> Just $ set 'o' (last cs) m

createCave :: [Text] -> Cave
createCave = set '+' start . (foldl' (flip rockify) <$> parseCave <*> id) . fmap (parse pathP)

solve :: Cave -> [Cave]
solve = fmap fromJust . takeWhile isJust . iterate' step . Just

solution1 :: IO Int
solution1 = pred . length . solve . createCave <$> input
-- 913

addRow :: Char -> Cave -> Cave
addRow c = (|>) <$> id <*> (`replicate` c) . width

solution2 :: IO Int
solution2 = length . solve . addRow '#' . addRow '.' . createCave <$> input
-- 30762