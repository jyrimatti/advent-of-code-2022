{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Day16 where
import Universum hiding (State, head, tail, intercalate, last, filter)
import Data.Text (Text, pack, intercalate)
import Data.Maybe (fromJust)
import Data.List (head, last, tail, (\\))
import Data.Tuple.Extra (uncurry3)
import Data.Foldable.Extra (sumOn')
import Data.Sequence (adjust', filter, findIndexL, index)
import Data.Vector ((!), (//))
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char (letterChar)
import Text.Megaparsec.Char.Lexer (space, decimal)
import Control.Applicative.Combinators (sepBy1)
import Algorithm.Search (aStar)

input :: IO [Text]
input = lines <$> readFile "input/input16.txt"

type Parser = Parsec () Text

data Valve = Valve {
     name    :: Text,
     flow    :: Int,
     tunnels :: [Text],
     open    :: Bool
} deriving (Show, Eq, Ord)

data State = State {
    valves     :: Seq Valve,
    current    :: Vector Text,
    minute     :: Int,
    closedFlow :: Int
} deriving (Show, Eq, Ord)

parse :: Parser a -> Text -> a
parse = fromJust ... parseMaybe

valveP :: Parser Valve
valveP = Valve <$> (pack <$> ("Valve " *> many letterChar))
               <*> (" has flow rate=" *> decimal <* "; tunnel" <* optional "s" <* " lead" <* optional "s" <* " to valve")
               <*> (fmap pack <$> (optional "s" *> " " *> sepBy1 (many letterChar) ", "))
               <*> pure False

neighboring :: Int -> State -> [State]
neighboring maxMinutes state@State{current,minute} = (\s -> s{minute = minute+1}) <$> foldl' (\states currentIndex -> concatMap (neighboringFor currentIndex) states) [state] [0..length current-1]

neighboringFor :: Int -> State -> [State]
neighboringFor currentIndex state@State{valves,current,minute} = if open valve || flow valve == 0 then move else openValve : move
    where cur             = current ! currentIndex
          Just valveIndex = findIndexL ((== cur) . name) valves
          valve           = valves `index` valveIndex
          openValve       = let valv = adjust' (\c -> c{ open = True }) valveIndex valves
                             in state{valves = valv,
                                      closedFlow = sumOn' flow (filter (not . open) valv) }
          move            = (\c -> state{current = current // [(currentIndex, c)]}) <$> tunnels valve

estimate :: Int -> State -> Int
estimate maxMinutes State{valves,closedFlow,minute}
  | maxMinutes == minute = 0
  | closedFlow == 0      = 0
  | otherwise            = sum $ concat $ take minute $ tail $ iterate (drop 1) $ sortOn Down $ toList $ flow <$> filter (not . open) valves

cost :: State -> State -> Int
cost from _ = closedFlow from

solution :: Int -> State -> Bool
solution maxMinutes State{closedFlow,minute} = closedFlow == 0 || maxMinutes == minute

solve :: Int -> State -> Maybe (Int, [State])
solve maxMinutes initial@State{valves} = second (initial:) <$> aStar (neighboring maxMinutes) cost (estimate maxMinutes) (solution maxMinutes) initial
    where flowTotal = sumOn' flow valves

pressure :: Int -> [State] -> Int
pressure minutes states = sum $ sumOn' flow . filter open . valves <$> take minutes (states <> repeat (last states))

solveFor :: Int -> [Text] -> [Text] -> Maybe Int
solveFor minutes actors = fmap (pressure minutes . snd) . solve minutes . (\valves -> State valves (fromList actors) 1 $ sumOn' flow (filter (not . open) valves)) . fromList . fmap (parse valveP)

solution1 :: IO (Maybe Int)
solution1 = solveFor 30 ["AA"] <$> input
-- 1862

solution2 :: IO (Maybe Int)
solution2 = solveFor 26 ["AA","AA"] <$> input
-- 2422