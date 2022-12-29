module Day02 where
import Universum
import Data.Text (split,Text, head)

input :: IO [[Char]]
input = fmap (fmap Data.Text.head . split (== ' ')) . lines <$> readFile "input/input02.txt"

pair :: [[b]] -> [(b, b)]
pair = fmap (\[a,b] -> (a,b))

normalize :: Char -> Char
normalize 'X' = 'A'
normalize 'Y' = 'B'
normalize 'Z' = 'C'
normalize a = a

score :: Char -> Int
score 'A' = 1
score 'B' = 2
score 'C' = 3
score 'X' = 1
score 'Y' = 2
score 'Z' = 3

run :: Char -> Char -> Int
run 'A' 'B' = 6
run 'B' 'C' = 6
run 'C' 'A' = 6
run 'A' 'A' = 3
run 'B' 'B' = 3
run 'C' 'C' = 3
run _ _ = 0

total :: Char -> Char -> Int
total a b = run a b + score b

solution1 :: IO Int
solution1 = sum . fmap (uncurry total) . pair . fmap (fmap normalize) <$> input
-- 12156

winnerFor :: Char -> Char
winnerFor 'A' = 'B' 
winnerFor 'B' = 'C'
winnerFor 'C' = 'A'

loserFor :: Char -> Char
loserFor 'A' = 'C'
loserFor 'B' = 'A'
loserFor 'C' = 'B'

mapToAction :: Char -> Char -> (Char, Char)
mapToAction a 'X' = (a, loserFor a)
mapToAction a 'Y' = (a, a)
mapToAction a 'Z' = (a, winnerFor a)

solution2 :: IO Int
solution2 = sum . fmap (uncurry total . uncurry mapToAction) . pair <$> input
-- 10835