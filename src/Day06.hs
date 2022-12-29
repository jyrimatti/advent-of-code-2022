module Day06 where
import Universum hiding (tail)
import Data.Text (Text, unpack)
import Data.List.Extra (anySame)
import Data.List (findIndex, tail)
import Data.Maybe (fromJust)

input :: IO [Char]
input = unpack <$> readFile "input/input06.txt"

isMarker :: Eq a => Int -> [a] -> Bool
isMarker = not . anySame ... take

solve :: Int -> [Char] -> Int
solve x = (+x) . fromJust . findIndex (isMarker x) . iterate tail

solution1 :: IO Int
solution1 = solve 4 <$> input
-- 1080

solution2 :: IO Int
solution2 = solve 14 <$> input
-- 3645
