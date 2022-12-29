module Day04 where
import Universum 
import Data.Text (Text, split, unpack)
import Text.Read (read)
import Data.List (isSubsequenceOf, intersect)
import Data.Tuple.Extra (both)
import Control.Conditional ((<||>))

input :: IO [Text]
input = lines <$> readFile "input/input04.txt"

toPair :: [b] -> (b, b)
toPair [a,b] = (a,b)

parse :: Text -> ((Natural,Natural),(Natural,Natural))
parse = toPair . fmap (toPair . fmap (read . unpack) . split (== '-')) . split (== ',')

containsOther :: Eq a => [a] -> [a] -> Bool
containsOther = (<||>) <$> isSubsequenceOf <*> flip isSubsequenceOf

solve :: ([Natural] -> [Natural] -> Bool) -> [Text] -> Int
solve f = length . filter id . fmap (uncurry f . both (uncurry enumFromTo) . parse)

solution1 :: IO Int
solution1 = solve containsOther <$> input
-- 573

overlap :: Eq a => [a] -> [a] -> Bool
overlap = not . null ... intersect

solution2 :: IO Int
solution2 = solve overlap <$> input
-- 867