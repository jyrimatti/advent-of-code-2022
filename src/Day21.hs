{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day21 where
import Universum hiding (try, ap)
import Data.Text (Text, pack)
import Data.Maybe (fromJust)
import Data.Map.Strict ((!), insert)
import Text.Megaparsec (Parsec, parseMaybe, try)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (letterChar, string)

input :: IO [Text]
input = lines <$> readFile "input/input21.txt"

type Parser = Parsec () Text

type Numtype = Int

data Op = Yell Numtype | Add Text Text | Sub Text Text | Mul Text Text | Div Text Text | Me

data Job = Job {
    name :: Text,
    op   :: Op
}

parse :: Parser a -> Text -> a
parse = fromJust ... parseMaybe

arithP :: Text -> Parser (Text,Text)
arithP op = try ((,) <$> (pack <$> (many letterChar <* string (" " <> op <> " ")))
                     <*> (pack <$> many letterChar))

jobP :: Parser Job
jobP = Job <$> (pack <$> many letterChar <* ": ") <*> ((Yell <$> decimal) <|>
                                                       (uncurry Add <$> arithP "+") <|>
                                                       (uncurry Sub <$> arithP "-") <|>
                                                       (uncurry Mul <$> arithP "*") <|>
                                                       (uncurry Div <$> arithP "/"))

apply :: Maybe Numtype -> Map Text Job -> Job -> Maybe Numtype
apply r m (Job "root" op@(Add a b)) = let
        aa = apply Nothing m (m ! a)
        bb = apply Nothing m (m ! b)
    in case (aa,bb) of
        (Just aaa, Just bbb) -> Just (aaa+bbb)
        (Just aaa, _)  -> apply aa m (m ! b)
        (_, Just bbb)  -> apply bb m (m ! a)
apply r m Job{op} = applyOp r m op

ap :: Maybe Numtype -> Map Text Job -> Text -> Maybe Numtype
ap r m k = apply r m (m ! k)

applyOp :: Maybe Numtype -> Map Text Job -> Op -> Maybe Numtype
applyOp r m (Yell value) = Just value
applyOp r m  Me          = r
applyOp r m (Add a b) = case (ap Nothing m a,ap Nothing m b) of
        (Nothing, _) | isNothing r -> Nothing
        (_, Nothing) | isNothing r -> Nothing
        (Just aa, Just bb)         -> Just $ aa + bb
        (Just aa, _)               -> ap ((-) <$> r <*> Just aa) m b
        (_, Just bb)               -> ap ((-) <$> r <*> Just bb) m a
applyOp r m (Sub a b) = case (ap Nothing m a,ap Nothing m b) of
        (Nothing, _) | isNothing r -> Nothing
        (_, Nothing) | isNothing r -> Nothing
        (Just aa, Just bb)         -> Just $ aa - bb
        (Just aa, _)               -> ap ((-) <$> Just aa <*> r) m b
        (_, Just bb)               -> ap ((+) <$> Just bb <*> r) m a
applyOp r m (Mul a b) = case (ap Nothing m a,ap Nothing m b) of
        (Nothing, _) | isNothing r -> Nothing
        (_, Nothing) | isNothing r -> Nothing
        (Just aa, Just bb)         -> Just $ aa * bb
        (Just aa, _)               -> ap (div <$> r <*> Just aa) m b
        (_, Just bb)               -> ap (div <$> r <*> Just bb) m a
applyOp r m (Div a b) = case (ap Nothing m a,ap Nothing m b) of
        (Nothing, _) | isNothing r -> Nothing
        (_, Nothing) | isNothing r -> Nothing
        (Just aa, Just bb)         -> Just $ aa `div` bb
        (Just aa, _)               -> ap (div <$> Just aa <*> r) m b
        (_, Just bb)               -> ap ((*) <$> Just bb <*> r) m a

mkMap :: [Text] -> Map Text Job
mkMap = fromList . fmap (((,) <$> name <*> id) . parse jobP)

solve :: Map Text Job -> Numtype
solve m = fromJust $ apply Nothing m $ m ! "root"

solution1 :: IO Numtype
solution1 = solve . mkMap <$> input
-- 160274622817992

solution2 :: IO Numtype
solution2 = solve . insert "humn" (Job "humn" Me) . mkMap <$> input
-- 3087390115721
