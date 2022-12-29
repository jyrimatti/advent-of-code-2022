{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day25 where
import Universum hiding (tail)
import Text.Read (read)
import Data.Text (Text, unpack)
import Data.List (tail, scanl')
import Data.Maybe (fromJust)
import Data.Char (digitToInt)
import Data.Foldable.Extra (sumOn')

input :: IO [[Char]]
input = fmap unpack . lines <$> readFile "input/input25.txt"

toDigit :: Char -> Int
toDigit '=' = -2
toDigit '-' = -1
toDigit '0' = 0
toDigit '1' = 1
toDigit '2' = 2

fromDigit :: Int -> Char
fromDigit (-2) = '='
fromDigit (-1) = '-'
fromDigit    0 = '0'
fromDigit    1 = '1'
fromDigit    2 = '2'

fromSnafu :: [Char] -> [Char]
fromSnafu x = show $ sumOn' (uncurry go) $ zip [0..] $ reverse x
    where go p c = 5^p * toDigit c

solve :: Int -> Int -> (Int,Char)
solve   0 pow = (0,' ')
solve num pow = (num - mul * 5^pow, fromDigit mul)
    where rem = num `mod` 5^(pow+1) `div` 5^pow
          mul | rem == 0  = 0
              | rem == 1  = 1
              | rem == 2  = 2
              | rem == 3  = -2
              | rem == 4  = -1
              | otherwise = 0

toSnafu :: [Char] -> [Char]
toSnafu = reverse . takeWhile (/= ' ') . fmap snd . tail . flip (scanl' (solve . fst)) [0..] . (,' ') . read

solution1 :: IO [Char]
solution1 = toSnafu . show . sumOn' (read . fromSnafu) <$> input
-- 2=001=-2=--0212-22-2
