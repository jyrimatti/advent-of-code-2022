{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Day11 where
import Universum hiding (tail,last)
import Text.Megaparsec (Parsec, parseMaybe, sepBy1, manyTill, eitherP)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (newline, letterChar)
import Data.Maybe (fromJust)
import Data.Sequence (Seq, index, adjust', (|>))
import Data.List.Extra (split)
import Data.Tuple.Extra (dupe)

input :: IO [Text]
input = fmap unlines . split (== "") . lines <$> readFile "input/input11.txt"

type Parser = Parsec () Text

parse :: Parser a -> Text -> a
parse = fromJust ... parseMaybe

type MonkeyIndex = Int
type WorryLevel = Natural
type Adjuster = Seq Monkey -> WorryLevel -> WorryLevel

data Op = Add | Mul

data Monkey = Monkey {
    items       :: Seq WorryLevel,
    operation   :: (Op, Either Int Text),
    divisibleBy :: Int,
    ifTrue      :: MonkeyIndex,
    ifFalse     :: MonkeyIndex,
    inspections :: Int
}

mkOperation :: Op -> Either Int Text -> WorryLevel -> WorryLevel
mkOperation Add (Right _) = (*2)
mkOperation Add (Left x)  = (+ fromIntegral x)
mkOperation Mul (Right _) = uncurry (*) . dupe
mkOperation Mul (Left x)  = (* fromIntegral x)

isDivisibleBy :: WorryLevel -> Int -> Bool
isDivisibleBy x = (== 0) . (fromIntegral x `rem`)

monkeyP :: Parser Monkey
monkeyP = Monkey <$> (fromList <$> ("Monkey " *> decimal *> ":" *> newline *> "  Starting items: " *> sepBy1 decimal ", " <* newline))
                 <*> ((,) <$> ("  Operation: new = old " *> (Add <$ "+" <|> Mul <$ "*")) <*> (" " *> eitherP decimal "old" <* newline))
                 <*> ("  Test: divisible by " *> decimal <* newline)
                 <*> ("    If true: throw to monkey " *> decimal <* newline)
                 <*> ("    If false: throw to monkey " *> decimal <* newline)
                 <*> pure 0

handleItem :: Adjuster -> MonkeyIndex -> WorryLevel -> Seq Monkey -> Seq Monkey
handleItem adjuster currentMonkey item monkeys = let
    Monkey{operation, divisibleBy, ifTrue, ifFalse} = monkeys `index` currentMonkey
    newWorry     = adjuster monkeys $ uncurry mkOperation operation item
    targetMonkey = if isDivisibleBy newWorry divisibleBy then ifTrue else ifFalse
 in   adjust' (\m -> m{items = empty, inspections = inspections m + 1}) currentMonkey
    $ adjust' (\m -> m{items = items m |> newWorry}) targetMonkey
      monkeys

turn :: Adjuster -> MonkeyIndex -> Seq Monkey -> Seq Monkey
turn adjuster monkey = foldl' (flip $ handleItem adjuster monkey) <$> id <*> items . (`index` monkey)

handleRound :: Adjuster -> Seq Monkey -> Seq Monkey
handleRound adjuster monkeys = foldl' (flip $ turn adjuster) monkeys [0..length monkeys-1]

handleRounds :: Adjuster -> Int -> [Text] -> Seq Monkey
handleRounds adjuster rounds = flip (foldl' (\ms _ -> handleRound adjuster ms)) [1..rounds] . fmap (parse monkeyP) . fromList

adjustWorry :: Adjuster
adjustWorry _ = floor . (/ 3) . fromIntegral

solve :: Seq Monkey -> Int
solve = product . take 2 . sortOn Down . toList . fmap inspections

solution1 :: IO Int
solution1 = solve . handleRounds adjustWorry 20 <$> input
-- 119715

adjustWorry2 :: Adjuster
adjustWorry2 = flip rem ... fromIntegral . product . fmap divisibleBy

solution2 :: IO Int
solution2 = solve . handleRounds adjustWorry2 10000 <$> input
-- 18085004878
