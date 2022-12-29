{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Day15 where
import Universum hiding (minimum, maximum)
import Data.Text (Text)
import Data.Maybe (fromJust)
import Data.List (minimum,maximum)
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Char (space)

input :: IO [Text]
input = lines <$> readFile "input/input15.txt"

type Parser = Parsec () Text
type Coord = (Int,Int)
type SensorAndBeacon = (Coord,Coord)

parse :: Parser a -> Text -> a
parse = fromJust ... parseMaybe

sensorAndBeaconP :: Parser SensorAndBeacon
sensorAndBeaconP = (,) <$> ((,) <$> ("Sensor at x=" *> signed space decimal) <*> (", y=" *> signed space decimal))
                       <*> ((,) <$> (": closest beacon is at x=" *> signed space decimal) <*> (", y=" *> signed space decimal))

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (a,b) (c,d) = abs (a-c) + abs (b-d)

isBeacon :: [SensorAndBeacon] -> Coord -> Bool
isBeacon ss c = any ((c ==) . snd) ss

candidates :: Int -> [SensorAndBeacon] -> [Coord]
candidates yy ss = filter (not . isBeacon ss) $ fmap (,yy) [col1..col2]
    where maxRange = maximum $ fmap (uncurry manhattan) ss
          col1     = flip (-) maxRange $ minimum $ fmap (fst . fst) ss
          col2     = flip (+) maxRange $ maximum $ fmap (fst . fst) ss

inRange :: Coord -> SensorAndBeacon -> Bool
inRange candidate (sensor,beacon) = manhattan sensor candidate <= manhattan sensor beacon

solution1 :: IO Int
solution1 = length . (filter <$> flip (any . inRange) <*> candidates 2000000) . fmap (parse sensorAndBeaconP) <$> input
-- 5716881

limit :: Int
limit = 4000000

justOutside :: SensorAndBeacon -> [Coord]
justOutside (sensor@(x,y),beacon) = filter (\(a,b) -> a >= 0 && b >= 0 && a <= limit && b <= limit) $
                                    [(x+dx,y+dy) | dx        <- [-dist .. dist],
                                                   remaining <- [dist - abs dx],
                                                   dy        <- [-1*remaining, remaining]]
    where dist = 1 + manhattan sensor beacon

tuningFreq :: Coord -> Int
tuningFreq (x,y) = x*4000000 + y

solution2 :: IO Int
solution2 = tuningFreq . fromJust . (find <$> not ... flip (any . inRange) <*> concatMap justOutside) . fmap (parse sensorAndBeaconP) <$> input
-- 10852583132904