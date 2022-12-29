{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use maybe" #-}
module Day08 where
import Universum hiding (tail, zip, takeWhile, all, init, last, take, drop, reverse, head, maximum)
import Data.Text (Text, lines)
import Data.Char ( digitToInt )
import Data.Matrix (Matrix, fromLists, takeRow, takeColumn, rows, cols, imap, toList)
import Data.Vector (tail, zip, takeWhile, all, init, last, take, drop, reverse, head, findIndex)
import Data.FoldApp (allOf, FoldrApp, foldrApp, sumOf, productOf)
import Data.Foldable (maximum)

-- Data.FoldApp has this, but does not export it...
anyOf :: FoldrApp Bool Bool f => f
anyOf = foldrApp (||) False

input :: IO [[Char]]
input = fmap Universum.toList . lines <$> readFile "input/input08.txt"

heights :: [[Char]] -> Matrix Int
heights = fromLists . fmap (fmap digitToInt)

visibility :: Vector Int -> Bool
visibility = (\height -> all (< height)) <$> head <*> tail

isVisible, visibleLeft, visibleRight, visibleTop, visibleBottom :: (Int,Int) -> Matrix Int -> Bool
visibleLeft   (r,c) = (||) <$> const (c == 0)        <*> visibility . reverse . take (c+1) . (`takeRow` r)
visibleRight  (r,c) = (||) <$>       (c+1 ==) . cols <*> visibility .           drop c     . (`takeRow` r)
visibleTop    (r,c) = (||) <$> const (r == 0)        <*> visibility . reverse . take (r+1) . (`takeColumn` c)
visibleBottom (r,c) = (||) <$>       (r+1 ==) . rows <*> visibility .           drop r     . (`takeColumn` c)

isVisible c = anyOf <$> visibleLeft c <*> visibleTop c <*> visibleRight c <*> visibleBottom c

mapTo :: ((Int,Int) -> Matrix Int -> a) -> Matrix Int -> Matrix a
mapTo f m = imap (\c _ -> f c m) m

solution1 :: IO Int
solution1 = length . filter id . Data.Matrix.toList . mapTo isVisible . heights <$> input
-- 1812

score :: Vector Int -> Int
score = length ... (\height v -> fromMaybe v $ (\i -> take (i+1) v) <$> findIndex (>= height) v) <$> head <*> tail

scenicScore, scoreLeft, scoreRight, scoreTop, scoreBottom :: (Int,Int) -> Matrix Int -> Int
scoreLeft   (r,c) = score . reverse . take (c+1) . (`takeRow` r)
scoreRight  (r,c) = score .           drop c     . (`takeRow` r)
scoreTop    (r,c) = score . reverse . take (r+1) . (`takeColumn` c)
scoreBottom (r,c) = score .           drop r     . (`takeColumn` c)

scenicScore c = productOf <$> scoreLeft c <*> scoreTop c <*> scoreRight c <*> scoreBottom c

solution2 :: IO Int
solution2 = maximum . Data.Matrix.toList . mapTo scenicScore . heights <$> input
-- 315495