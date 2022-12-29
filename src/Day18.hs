{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Day18 where
import Universum hiding (head, tail, maximum)
import Data.Text (Text, split, unpack)
import Data.List.Extra (maximum)
import Data.Set (member)
import Data.Tuple.Extra (fst3, snd3, thd3)
import Text.Read (read)
import Algorithm.Search (aStar)

input :: IO [Text]
input = lines <$> readFile "input/input18.txt"

type Coord = (Int,Int,Int)
type Space = Set Coord

parse :: Text -> Coord
parse = (\[x,y,z] -> (x,y,z)) . fmap (fromIntegral . read @Natural . unpack) . split (== ',')

adjacent :: Coord -> [Coord]
adjacent (x,y,z) = [(x+dx,y+dy,z+dz) | dx <- [-1..1],
                                       dy <- [-1..1],
                                       dz <- [-1..1],
                                       abs dx + abs dy + abs dz == 1]

isFree :: Space -> Coord -> Bool
isFree s = not . (`member` s)

free :: Space -> Coord -> [Coord]
free s = filter (isFree s) . adjacent

surfaceArea :: Space -> Int
surfaceArea = length ... concatMap <$> free <*> id

solution1 :: IO Int
solution1 = surfaceArea . fromList . fmap parse <$> input
-- 3454

allAir :: Space -> [Coord]
allAir space = [(x,y,z) | x <- [0..(maximum $ fmap fst3 sp)],
                          y <- [0..(maximum $ fmap snd3 sp)],
                          z <- [0..(maximum $ fmap thd3 sp)],
                          isFree space (x,y,z)]
    where sp = toList space

neighboring :: Space -> Coord -> [Coord]
neighboring space = filter (isFree space) <$> adjacent

estimate :: Coord -> Float
estimate (x,y,z) = sqrt (fromIntegral $ x*x + y*y + z*z)

airPocketCoords :: Space -> [Coord]
airPocketCoords space = filter (isNothing . aStar (neighboring space) (const . const 1) estimate (== (0,0,0))) $ allAir space

solution2 :: IO Int
solution2 = ((-) <$> surfaceArea <*> surfaceArea . fromList . airPocketCoords) . fromList . fmap parse <$> input
-- 2014
