module Day12 where
import Universum
import Data.Text (Text)
import Data.Matrix (Matrix, fromLists, cols, rows, (!))
import Data.Maybe (fromJust)
import Algorithm.Search (bfs)

input :: IO (Matrix Char)
input = fromLists . fmap toList . lines <$> readFile "input/input12.txt"

type Coord = (Int,Int)

valid :: Matrix Char ->Coord -> Bool
valid _ (r,_) | r < 0       = False
valid _ (_,c) | c < 0       = False
valid m (r,_) | r >= rows m = False
valid m (_,c) | c >= cols m = False
valid _ _                   = True

height :: Matrix Char -> Coord -> Char
height m c | m ! c == 'E' = 'z'
height m c                = m ! c

validNext :: Matrix Char -> Coord -> Coord -> Bool
validNext m current candidate = m ! current == 'S' || height m candidate <= succ (height m current)

neighboring :: Matrix Char -> Coord -> [Coord]
neighboring m current@(x,y) = filter (validNext m current) $ filter (valid m) [(x-1,y), (x,y-1), (x+1,y), (x,y+1)]

solution :: Matrix Char -> Coord -> Bool
solution = (== 'E') ... (!)

elements :: Matrix Char -> [Coord]
elements m = [(r,c) | r <- [0..rows m-1], c <- [0..cols m-1]]

initial :: Matrix Char -> Coord
initial m = fromJust $ find ((== 'S') . (m !)) $ elements m

solve :: Matrix Char -> Maybe [Coord]
solve = bfs <$> neighboring <*> solution <*> initial

solution1 :: IO Int
solution1 = length . fromJust . solve <$> input
-- 517

solve2 :: Matrix Char -> [[Coord]]
solve2 m = mapMaybe (bfs (neighboring m) (solution m)) $ filter ((== 'a') . height m) (elements m)

solution2 :: IO Int
solution2 = minimum . fromList . fmap length . solve2 <$> input
-- 512