{-# LANGUAGE OverloadedStrings #-}
module Day20 where
import Universum
import Data.Text (Text, unpack)
import Text.Read (read)
import Data.Sequence (findIndexL, index, insertAt, deleteAt)
import Data.List.Extra (sumOn')

input :: IO (Seq (Int,Int))
input = fromList . zip [0..] . fmap (read . unpack) . lines <$> readFile "input/input20.txt"

act :: Seq (Int,Int) -> Int -> Seq (Int,Int)
act seq i = if toMove == 0 then seq else insertAt newIndex e $ deleteAt indexToMove seq
    where Just indexToMove = findIndexL ((==i) . fst) seq
          e@(_,toMove)     = seq `index` indexToMove
          newIndex         = (indexToMove + toMove) `mod` (length seq - 1)

move :: Int -> Seq (Int,Int) -> Seq (Int,Int)
move times seq = foldl' act seq $ concat $ replicate times [0..length seq - 1]

grove :: [Int]
grove = [1000, 2000, 3000]

solve :: Seq (Int, Int) -> Int
solve = sumOn' snd . take (length grove) . fmap snd . filter ((`elem` grove) . fst) . zip [0..] . dropWhile ((/= 0) . snd) . cycle . toList

solution1 :: IO Int
solution1 = solve . move 1 <$> input
-- 3346

key :: Int
key = 811589153

solution2 :: IO Int
solution2 = solve . move 10 . fmap (second (*key)) <$> input
-- 4265712588168
