{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Day10 where
import Universum hiding (State, cycle)
import Data.Text (Text, pack)
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Data.Maybe (fromJust)
import Text.Megaparsec.Char (space)
import Data.List (scanl', singleton)
import Data.Tuple.Extra (snd3, fst3, thd3, uncurry3)
import Data.List.Extra (chunksOf, sumOn')

input :: IO [Text]
input = lines <$> readFile "input/input10.txt"

type Parser = Parsec () Text

parse :: Parser a -> Text -> a
parse = fromJust ... parseMaybe

data Command = Noop | Addx Int

data State = State {
    cycle          :: Int,
    x              :: Int,
    signalStrength :: Int,
    pixel          :: Int
}

commandP :: Parser [Command]
commandP = [Noop] <$ "noop" <|> 
           (Noop :) . singleton . Addx <$> ("addx " *> signed space decimal)

relevantCycles :: [Int]
relevantCycles = [20, 60, 100, 140, 180, 220]

act :: Int -> Command -> Int
act state Noop     = state
act state (Addx x) = state + x

draw :: State -> Char
draw State{..} | pixel >= x-1 && pixel <= x+1 = '#'
draw _                                        = '.'

mkState :: [Int] -> [State]
mkState = fmap (State <$> fst <*> snd <*> uncurry (*) <*> (`mod` 40) . pred . fst) . zip [1..]

process :: [Text] -> [State]
process = mkState . scanl' act 1 . concatMap (parse commandP)

solution1 :: IO Int
solution1 = sumOn' signalStrength . filter ((`elem` relevantCycles) . cycle) . process <$> input
-- 12460

solution2 :: IO [[Char]]
solution2 = chunksOf 40 . fmap draw . process <$> input
-- ####.####.####.###..###...##..#..#.#....
-- #.......#.#....#..#.#..#.#..#.#.#..#....
-- ###....#..###..#..#.#..#.#..#.##...#....
-- #.....#...#....###..###..####.#.#..#....
-- #....#....#....#....#.#..#..#.#.#..#....
-- ####.####.#....#....#..#.#..#.#..#.####.
-- == EZFPRAKL