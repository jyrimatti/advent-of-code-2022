{-# LANGUAGE OverloadedStrings #-}
module Day07 where
import Universum hiding (show, fold, init, toList, maximum, minimum)
import Data.Text (Text, pack, unpack)
import Data.Tree (Tree (Node, subForest, rootLabel))
import Data.Functor.Foldable (fold, Recursive (para))
import Data.Functor.Base (TreeF(..), ForestF)
import Text.Megaparsec (Parsec, parseMaybe, manyTill)
import Text.Megaparsec.Char (letterChar, char, asciiChar, newline, alphaNumChar, punctuationChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Maybe (fromJust)
import Text.Parsec (endOfLine)
import Text.Show (Show(show))
import Data.List.Extra (init)
import Data.Foldable (Foldable(toList, minimum, maximum))

input :: IO Text
input = (<> "\n") <$> readFile "input/input07.txt"

parse :: Parser a -> Text -> a
parse = fromJust ... parseMaybe

type Parser = Parsec () Text
type FileSystem = Tree Inode
type CurrentWorkingDirectory = [Text]

data Command = CD Text | CDup | CDroot | LS [Inode]
  deriving Show

data Inode = File Natural Text | Dir Text
  deriving Eq

instance Show Inode where
  show (File size name) = show size <> " " <> unpack name
  show (Dir name      ) = unpack name
    
commandP :: Parser Command
commandP = "$ " *> ((CDup <$ "cd .." <* newline) <|>
                    (CDroot <$ "cd /" <* newline) <|>
                    (CD . pack <$> ("cd " *> manyTill (letterChar <|> punctuationChar) newline)) <|>
                    (LS <$> ("ls" *> newline *> many inodeP)))

inodeP :: Parser Inode
inodeP = (File <$> (decimal <* " ") <*> (pack <$> manyTill (letterChar <|> punctuationChar) newline)) <|>
         (Dir . pack <$> ("dir " *> manyTill (letterChar <|> punctuationChar) newline))

updateFS :: FileSystem -> CurrentWorkingDirectory -> (FileSystem -> FileSystem) -> FileSystem
updateFS fs [] f = f fs
updateFS fs (top:rest) f = fs { subForest = modify <$> subForest fs }
  where modify dir | rootLabel dir == Dir top = updateFS dir rest f
        modify dir                            = dir

root :: (FileSystem, CurrentWorkingDirectory)
root = (Node (Dir "/") [], [])

build :: (FileSystem, CurrentWorkingDirectory) -> Command -> (FileSystem,CurrentWorkingDirectory)
build (fs, _)   CDroot      = root
build (fs, dir) (LS inodes) = (updateFS fs dir (\x -> x { subForest = fmap (`Node` []) inodes }), dir)
build (fs, dir) (CD subdir) = (fs, dir <> [subdir])
build (fs, dir) CDup        = (fs, init dir)

directorySizes :: TreeF Inode (Tree Inode, Tree (Maybe Text,Natural)) -> Tree (Maybe Text,Natural)
directorySizes (NodeF x@(File s n) []) = Node (Nothing,s) []
directorySizes (NodeF x@(Dir n) xs) = Node (Just n, sum $ fmap (snd . rootLabel) recvals) $ filter isDirectory recvals
  where recvals = fmap snd xs
        isDirectory = isJust . fst . rootLabel

dirSizes :: Text -> [(Maybe Text, Natural)]
dirSizes = toList . para directorySizes . fst . foldl' build root . parse (many commandP)

spaceNeeded :: [Natural] -> Natural
spaceNeeded = (30000000 -) . (70000000 -) . maximum

solution1 :: IO Natural
solution1 = sum . filter (<= 100000) . fmap snd . dirSizes <$> input
-- 1350966

solution2 :: IO Natural
solution2 = minimum . (filter <$> (<) . spaceNeeded <*> id) . fmap snd . dirSizes <$> input
-- 6296435