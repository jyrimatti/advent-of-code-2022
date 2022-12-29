{-# LANGUAGE OverloadedStrings, TupleSections, RecordWildCards #-}
module Day19 where
import Universum hiding (State, toList, maximum)
import Data.Text (Text)
import Data.Maybe (fromJust)
import Data.List(maximum, filter, iterate', lookup, (!!))
import Data.List.Extra (enumerate, nubOrd, sumOn', productOn')
import Data.Map.Strict (unions, mapWithKey, (!), unionWith, toList, singleton, insert)
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Applicative.Combinators (sepBy1)

input :: IO [Text]
input = lines <$> readFile "input/input19.txt"

type Parser = Parsec () Text

parse :: Parser a -> Text -> a
parse = fromJust ... parseMaybe

data Mineral = Ore | Clay | Obsidian | Geode
    deriving (Eq, Ord, Show, Enum, Bounded)

type Cost = (Mineral,Int)

data Blueprint = Blueprint {
    idnum :: Int,
    costs :: Map Mineral [Cost]
} deriving Show

mineralP :: Parser Mineral
mineralP = (Ore <$ "ore") <|> (Clay <$ "clay") <|> (Obsidian <$ "obsidian") <|> (Geode <$ "geode")

costP :: Parser Cost
costP = flip (,) <$> decimal <*> (" " *> mineralP)

robotP :: Parser (Mineral,[Cost])
robotP = (,) <$> ("Each " *> mineralP) <*> (" robot costs "*> sepBy1 costP " and ")

blueprintP :: Parser Blueprint
blueprintP = Blueprint <$> ("Blueprint " *> decimal <* ": ") <*> (fromList <$> sepBy1 robotP ". " <* ".")

data State = State {
    minute   :: Int,
    robots   :: Map Mineral Int,
    minerals :: Map Mineral Int
} deriving (Eq, Ord, Show)

incrementMinerals :: Map Mineral Int -> Map Mineral Int -> Map Mineral Int
incrementMinerals robots = mapWithKey (\k v -> v + robots ! k)

newRobots :: Map Mineral Int -> Map Mineral [Cost] -> [(Mineral,[Cost])]
newRobots minerals = filter (all (\(mineral,price) -> minerals ! mineral >= price) . snd) . toList

neighboring :: Int -> Blueprint -> State -> [State]
neighboring minutes Blueprint{..} State{..} | minute == minutes     = []
neighboring minutes Blueprint{..} State{..} | minute == (minutes-1) = [State (minute+1) robots (incrementMinerals robots minerals)]
neighboring minutes Blueprint{..} State{..} = onlyIncrement <> [State (minute+1) r (incrementMinerals robots m) | (r,m) <- zip updatedRobots updatedMinerals]
    where robotCandidates  = filter (\x -> minute < (minutes-2) || fst x == Geode) $ newRobots minerals costs
          newRobs | isJust (Geode    `lookup` robotCandidates) = filter ((== Geode)    . fst) robotCandidates
                  | isJust (Obsidian `lookup` robotCandidates) = filter ((== Obsidian) . fst) robotCandidates
                  | otherwise                                  = robotCandidates
          onlyIncrement    = [State (minute+1) robots (incrementMinerals robots minerals)]
          updatedMinerals  = unionWith (-) minerals . fromList . snd <$> newRobs
          updatedRobots    = unionWith (+) robots . uncurry singleton . second (const 1) <$> newRobs

initial :: State
initial = (State 0 <$> insert Ore 1 . fromList <*> fromList) $ fmap (,0) enumerate

guessedLimitByTrialAndErrorSadFace :: Int
guessedLimitByTrialAndErrorSadFace = 4

filterByGeodeRobots :: [State] -> [State]
filterByGeodeRobots s = if null s || maxGeodesobots == 0 then s else filter ((>= maxGeodesobots - guessedLimitByTrialAndErrorSadFace) . (! Geode) . robots) s
    where maxGeodesobots = maximum $ fmap ((! Geode) . robots) s

solve :: Int -> Blueprint -> [State]
solve minutes bp = (!! minutes) $ iterate' (nubOrd . filterByGeodeRobots ... concatMap $ neighboring minutes bp) [initial]

solution1 :: IO Int
solution1 = sumOn' ( ((*) <$> idnum <*> maximum . fmap ((! Geode) . minerals) . solve 24) . parse blueprintP) <$> input
-- 1653

solution2 :: IO Int
solution2 = productOn' (maximum . fmap ((! Geode) . minerals) . solve 32 . parse blueprintP) . take 3 <$> input
-- 4212
