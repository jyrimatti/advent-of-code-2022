module Day03 where
import Universum
import Data.Text (Text, unpack)
import Data.List.Extra (span, chunksOf)
import Data.Char (isAsciiUpper)
import Data.Set (intersection, empty)
import Data.List (foldl1')

type Item = Char

input :: IO [[Item]]
input = fmap unpack . lines <$> readFile "input/input03.txt"

parse :: [Item] -> ([Item],[Item])
parse = splitAt <$> (`div` 2) . length <*> id

priority :: Item -> Int
priority a | isAsciiUpper a = flip (-) 38 $ ord a
priority a                  = flip (-) 96 $ ord a

commonItems :: [[Item]] -> [Item]
commonItems = toList . foldl1' intersection . fmap fromList

sumOfTypes :: [[Item]] -> Int
sumOfTypes = sum . fmap priority . concat

solution1 :: IO Int
solution1 = sumOfTypes . fmap (commonItems . (\(a,b) -> [a,b]) . parse) <$> input
-- 8072

solution2 :: IO Int
solution2 = sumOfTypes . fmap commonItems . chunksOf 3 <$> input
-- 2567