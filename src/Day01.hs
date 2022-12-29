module Day01 where
import Universum
import Data.Text (Text, unpack)
import Data.List.Extra (split, maximumOn)
import Text.Read ( read )

input :: IO [[Int]]
input = fmap (fmap read) . split (== "") . fmap unpack . lines <$> readFile "input/input01.txt"

withMostTotal :: [[Int]] -> [Int]
withMostTotal = maximumOn sum

solution1 :: IO Int
solution1 = sum . withMostTotal <$> input
-- 71300

sorted :: [[Int]] -> [[Int]]
sorted = sortOn (Down . sum)

solution2 :: IO Int
solution2 = sum . take 3 . fmap sum . sorted <$> input
-- 209691