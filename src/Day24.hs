{-# LANGUAGE OverloadedStrings, TupleSections, RecordWildCards, FlexibleInstances, NamedFieldPuns, FlexibleContexts #-}
module Day24 where
import Universum hiding (last)
import Data.Text (Text, unpack)
import Data.Maybe (fromJust)
import Data.Matrix.Unboxed as M (fromLists, takeRow, toRows, Matrix, toLists, rows, dim)
import Data.Vector.Unboxed as V (elemIndex, Vector, fromList, elem, map)
import Data.List (last, filter)
import Data.FoldApp (allOf, FoldrApp, foldrApp)
import Algorithm.Search (aStar)
import qualified Data.Matrix

input :: IO [[Char]]
input = fmap unpack . lines <$> readFile "input/input24.txt"

type Coord = (Int,Int)

data Valley = Valley {
    blizzards :: V.Vector (Char,Coord),
    you        :: Coord,
    start      :: Coord,
    finish     :: Coord,
    dimensions :: (Int,Int),
    minute     :: Int
} deriving (Show, Eq, Ord)

toBlizzard :: Char -> Coord -> Maybe (Char,Coord)
toBlizzard '>' = Just . ('>',)
toBlizzard '<' = Just . ('<',)
toBlizzard '^' = Just . ('^',)
toBlizzard 'v' = Just . ('v',)
toBlizzard _   = const Nothing

parseValley :: [[Char]] -> Valley
parseValley = (Valley <$> V.fromList . catMaybes . Data.Matrix.toList . Data.Matrix.imap (flip toBlizzard) . Data.Matrix.fromLists . toLists
                      <*> (0,) . fromJust . elemIndex '.' . (`takeRow` 0)
                      <*> (0,) . fromJust . elemIndex '.' . (`takeRow` 0)
                      <*> ((,) <$> pred . rows <*> fromJust . elemIndex '.' . last . toRows )
                      <*> dim
                      <*> pure 0
                ) . fromLists

isBorder :: Valley -> Coord -> Bool
isBorder Valley{dimensions} (r,c) = r <= 0 || c <= 0 || r >= fst dimensions-1 || c >= snd dimensions-1

delta :: (Int,Int) -> Char -> (Int, Int)
delta v '<' = (0             ,snd v - 3)
delta v '>' = (0             ,-1*(snd v - 3))
delta v '^' = (    fst v - 3 ,0)
delta v 'v' = (-1*(fst v - 3),0)
delta _  _  = (0             ,0)

step :: Char -> (Int, Int)
step '<' = (0 ,-1)
step '>' = (0 ,1)
step '^' = (-1,0)
step 'v' = (1 ,0)
step  _  = (0 ,0)

blow :: Valley -> (Char,Coord) -> (Char,Coord)
blow v@Valley{..} (b,c@(row,col)) = (b,target)
    where (dr,dc) = delta dimensions b
          (sr,sc) = step b
          target  = if isBorder v (row+sr,col+sc) then (row+dr,col+dc) else (row+sr,col+sc)

blows :: Valley -> Valley
blows v = v{blizzards = V.map (blow v) (blizzards v)}

-- Data.FoldApp has this, but does not export it...
anyOf :: FoldrApp Bool Bool f => f
anyOf = foldrApp (||) False

neighboring :: Valley -> [Valley]
neighboring v = (\y -> vv{you = y, minute = minute+1}) <$> moves
    where vv@Valley{..} = blows v
          candidates    = bimap ((+) $ fst you) ((+) $ snd you) <$> fmap step ['<','^','v','>', ' ']
          moves         = filter ((&&) <$> (anyOf <$> (== start) <*> (== finish) <*> not . isBorder vv)
                                       <*> not . (`V.elem` V.map snd blizzards)) candidates

costs :: Valley -> Valley -> Int
costs _ _ = 1

estimate :: Valley -> Int
estimate Valley{..} = abs (fst you - fst finish) + abs (snd you - snd finish)

solution :: Valley -> Bool
solution = (==) <$> you <*> finish

solve :: Valley -> Valley
solve = last . snd . fromJust . aStar neighboring costs estimate solution

solution1 :: IO Int
solution1 = minute . solve . parseValley <$> input
-- 311

reverseValley :: Valley -> Valley
reverseValley v = v{start = finish v, finish = start v}

solution2 :: IO Int
solution2 = minute . solve . reverseValley . solve . reverseValley . solve . parseValley <$> input
-- 869
