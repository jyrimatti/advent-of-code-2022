{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Day23 where
import Universum hiding (fromList, tail, get, set, round, replicate)
import Data.Text (Text, unpack)
import Data.Maybe (fromJust)
import Data.Sequence (index, adjust', update, fromList, foldMapWithIndex, findIndicesL, (<|), (|>), replicate, dropWhileL, dropWhileR)
import Data.List (tail, elemIndices, (!!), iterate', findIndex)
import Data.List.Extra (enumerate)

input :: IO [[Char]]
input = fmap unpack . lines <$> readFile "input/input23.txt"

type Cave = Seq (Seq Char)
type Coord = (Int,Int)

get :: Coord -> Cave -> Char
get (row,col) = (`index` col) . (`index` row)

set :: Char -> Coord -> Cave -> Cave
set c (row,col) = adjust' (update col c) row

parseCave :: [[Char]] -> Cave
parseCave = fromList . fmap fromList

data Direction = N | S | W | E
    deriving (Enum, Bounded)

elves :: Cave -> [Coord]
elves = foldMapWithIndex (\i -> fmap (i,) . findIndicesL (== '#'))

candidates :: Direction -> Coord -> [Coord]
candidates N (row,col) = [(row-1,col+d) | d <- [-1..1]]
candidates S (row,col) = [(row+1,col+d) | d <- [-1..1]]
candidates E (row,col) = [(row+d,col+1) | d <- [-1..1]]
candidates W (row,col) = [(row+d,col-1) | d <- [-1..1]]

neighboring :: Coord -> [Coord]
neighboring (row,col) = [(row+dx,col+dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

half1 :: Cave -> [Direction] -> Coord -> Maybe Coord
half1 cave directions elf = if all (all (== '.') . fmap (`get` cave)) dirs
                            then Nothing
                            else (!! 1) <$> find (all (== '.') . fmap (`get` cave)) dirs
    where dirs      = fmap (`candidates` elf) directions

move :: (Coord,Coord) -> Cave -> Cave
move (elf,target) = set '.' elf . set '#' target

width :: Cave -> Int
width = length . (`index` 0)

addRows :: Char -> Cave -> Cave
addRows c = (\c r -> (r <| c) |> r) <$> id <*> (`replicate` c) . width

removeRows :: Cave -> Cave
removeRows = dropWhileR (all (== '.')) . dropWhileL (all (== '.'))

transposeSeq :: Seq (Seq a) -> Seq (Seq a)
transposeSeq = fmap fromList . fromList . transpose . toList . fmap toList

shrink :: Cave -> Cave
shrink = transposeSeq . removeRows . transposeSeq . removeRows

round :: Cave -> [Direction] -> (Cave,[Direction])
round originalCave directions = (newCave, tail directions)
    where cave         = addRows '.' $ fmap (\row -> '.' <| (row |> '.')) originalCave
          considerations  = ((,) <$> id <*> half1 cave (take 4 directions)) <$> elves cave
          proposals       = second fromJust <$> filter (isJust . snd) considerations
          allProposals    = fmap snd proposals
          uniqueProposals = filter ((== 1) . length . (`elemIndices` allProposals) . snd) proposals
          newCave         = shrink $ foldl' (flip move) cave uniqueProposals

act :: Cave -> [(Cave, [Direction])]
act cave = iterate' (uncurry round) (cave,cycle enumerate)

solution1 :: IO Int
solution1 = length . filter (== '.') . concatMap toList . toList . fst . (!!10) . act . parseCave <$> input
-- 4172

solution2 :: IO Int
solution2 = (+1) . fromJust . findIndex ((==) <$> fst <*> snd) . (zip <$> id <*> tail) . fmap fst . act . parseCave <$> input
-- 942
