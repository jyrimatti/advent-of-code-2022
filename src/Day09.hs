module Day09 where
import Universum hiding (head, last, tail)
import Data.Text (Text, split, head, unpack)
import Data.Char (isSpace)
import Text.Read (read)
import Data.List.Extra (nubOrd)
import Data.List (last, tail, head, iterate')
import qualified Data.List.Split as Data.List

toPair :: [b] -> (b, b)
toPair [a,b] = (a,b)

input :: IO [Text]
input = lines <$> readFile "input/input09.txt"

parse :: [Text] -> [(Char,Int)]
parse = fmap (bimap Data.Text.head (read . unpack) . toPair . split isSpace)

type Position = (Int,Int)

distance :: Position -> Position -> Int
distance (a,b) (c,d) = abs (a - c) + abs (b - d)

move :: Char -> Position -> Position
move 'U' (x,y) = (x,y+1)
move 'D' (x,y) = (x,y-1)
move 'L' (x,y) = (x-1,y)
move 'R' (x,y) = (x+1,y)

limit :: Int -> Int
limit = max (-1) . min 1

diagonalStep :: Position -> Position -> Position
diagonalStep (a,b) (c,d) = (limit $ a - c, limit $ b - d)

onSameRowOrCol :: Position -> Position -> Bool
onSameRowOrCol (a,b) (c,d) = a == c || b == d

act :: Char -> [Position] -> [Position]
act direction knots@(h:xs) = take (length knots) $ Data.List.head <$> iterate' actOnHeads (move direction h : xs)

actOnHeads :: [Position] -> [Position]
actOnHeads [a]                         = [a]
actOnHeads (hPos : tPos@(tx, ty) : xs) = let
    dist    = distance hPos tPos
    onSame  = onSameRowOrCol hPos tPos
    (dx,dy) = diagonalStep hPos tPos
    newT
      |     onSame && dist > 1 = (tx+dx,ty+dy)
      | not onSame && dist > 2 = (tx+dx,ty+dy)
      | otherwise              = tPos
 in newT : xs

flat :: [(Char,Int)] -> [Char]
flat = concatMap (replicate <$> snd <*> fst)

solve :: Int -> [Char] -> [Position]
solve knots = nubOrd . fmap last . scanl (flip act) (replicate knots (0,0))

solution1 :: IO Int
solution1 = length . solve 2 . flat . parse <$> input
-- 6023

solution2 :: IO Int
solution2 = length . solve 10 . flat . parse <$> input
-- 2533