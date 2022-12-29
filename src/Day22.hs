{-# LANGUAGE OverloadedStrings, TupleSections, RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day22 where
import Universum hiding (replicate,init,last,take,maximum,State)
import Data.Text (Text, unpack, length, replicate, take)
import Data.List (last, init, elemIndex, (!!))
import Data.Matrix (Matrix, fromLists, takeRow, (!))
import Data.Vector as V (elemIndex)
import Data.Foldable (maximum)
import Data.Maybe (fromJust)
import Data.Tuple.Extra (both)
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char.Lexer (decimal)


normalize :: [Text] -> [Text]
normalize x = take maxLength <$> fmap (<> replicate maxLength " ") x
    where maxLength = maximum $ fmap Data.Text.length x

input :: IO ([Text],Text)
input = ((,) <$> normalize . init . init <*> last) . lines <$> readFile "input/input22.txt"

type Parser = Parsec () Text

data Move = Forward Int | TurnLeft | TurnRight
    deriving Show

data Direction = R | D | L | U
    deriving (Show, Enum, Eq)

data State = State {
    map :: Matrix Char,
    loc :: (Int,Int),
    dir :: Direction
} deriving Show

type Coord = (Int,Int)
type Mapper = Coord -> Direction -> Maybe (Coord,Direction)
type Mapping = Int -> [(Coord,Direction)]
type Correspondence = Int -> Int

parse :: Parser a -> Text -> a
parse = fromJust ... parseMaybe

moveP :: Parser Move
moveP = (Forward <$> decimal) <|> (TurnLeft <$ "L") <|> (TurnRight <$ "R")

parseMap :: [Text] -> Matrix Char
parseMap = fromLists . fmap unpack

parseMoves :: Text -> [Move]
parseMoves = parse (many moveP)

start :: Matrix Char -> State
start = State <$> id <*> (0,) . fromJust . V.elemIndex '.' . (`takeRow` 0) <*> const R

turn :: Direction -> (Direction,Direction)
turn U = (L,R)
turn R = (U,D)
turn D = (R,L)
turn L = (D,U)

mappingFor :: (Int -> [(Coord,Direction)]) -> Coord -> Direction -> Maybe Int
mappingFor mappings c d = fst <$> find (isJust . find (== (c,d)) . snd) allMappings
    where allMappings = filter (not . null . snd) $ fmap ((,) <$> id <*> mappings) [1,11,12,2,21,22,3,31,32,4,41,42,5,51,52,6,61,62,7,71,72,8,81,82,9,91,92,10,101,102]

unStep :: Coord -> Direction -> (Coord,Direction)
unStep (row,col) D = ((row+1,col),U)
unStep (row,col) U = ((row-1,col),D)
unStep (row,col) R = ((row,col+1),L)
unStep (row,col) L = ((row,col-1),R)

doStep :: Coord -> Direction -> (Coord,Direction)
doStep (row,col) U = ((row-1,col),U)
doStep (row,col) D = ((row+1,col),D)
doStep (row,col) L = ((row,col-1),L)
doStep (row,col) R = ((row,col+1),R)

getMapping :: Mapping -> (Int -> Int) -> Coord -> Direction -> Maybe (Coord,Direction)
getMapping mapping correspondence c d = uncurry unStep <$> newCoord
    where fromEdge = mappingFor mapping c d
          toEdge = correspondence <$> fromEdge
          oldIndex = fromJust . Data.List.elemIndex c . fmap fst . mapping <$> fromEdge
          newCoord = (!!) <$> fmap (reverse . mapping) toEdge <*> oldIndex

mapping :: Mapping
mapping 11  = [((row,50) ,L) | row <-         [0..49]]
mapping 12  = [((row,50) ,L) | row <-         [50..99]]
mapping 2   = [((100,col),U) | col <- reverse [0..49]]
mapping 31  = [((row,0)  ,L) | row <-         [100..149]]
mapping 32  = [((row,0)  ,L) | row <-         [150..199]]
mapping 4   = [((199,col),D) | col <-         [0..49]]
mapping 5   = [((row,49) ,R) | row <- reverse [150..199]]
mapping 6   = [((149,col),D) | col <-         [50..99]]
mapping 71  = [((row,99) ,R) | row <- reverse [100..149]]
mapping 72  = [((row,99) ,R) | row <- reverse [50..99]]
mapping 8   = [((49,col) ,D) | col <-         [100..149]]
mapping 9   = [((row,149),R) | row <- reverse [0..49]]
mapping 101 = [((0,col)  ,U) | col <- reverse [100..149]]
mapping 102 = [((0,col)  ,U) | col <- reverse [50..99]]
mapping _   = []

correspondenceFlat :: Correspondence
correspondenceFlat 11  = 9
correspondenceFlat 12  = 72
correspondenceFlat 2   = 4
correspondenceFlat 31  = 71
correspondenceFlat 32  = 5
correspondenceFlat 4   = 2
correspondenceFlat 5   = 32
correspondenceFlat 6   = 102
correspondenceFlat 71  = 31
correspondenceFlat 72  = 12
correspondenceFlat 8   = 101
correspondenceFlat 9   = 11
correspondenceFlat 101 = 8
correspondenceFlat 102 = 6

correspondenceCube :: Correspondence
correspondenceCube 11  = 31
correspondenceCube 12  = 2
correspondenceCube 2   = 12
correspondenceCube 31  = 11
correspondenceCube 32  = 102
correspondenceCube 4   = 101
correspondenceCube 5   = 6
correspondenceCube 6   = 5
correspondenceCube 71  = 9
correspondenceCube 72  = 8
correspondenceCube 8   = 72
correspondenceCube 9   = 71
correspondenceCube 101 = 4
correspondenceCube 102 = 32

move :: Mapper -> Move -> State -> State
move mapper TurnLeft  s             = s{dir = fst $ turn (dir s)}
move mapper TurnRight s             = s{dir = snd $ turn (dir s)}
move mapper (Forward 1) s@State{..} = let (newLoc,newDir) = uncurry doStep $ fromMaybe (loc,dir) (mapper loc dir)
                                       in if map ! newLoc == '.' then s{loc = newLoc, dir = newDir} else s
move mapper (Forward x) s           = foldl' (const . move mapper (Forward 1)) s [1..x]

act :: Mapper -> State -> [Move] -> State
act mapper = foldl' (flip $ move mapper)

password :: (Int,Int) -> Direction -> Int
password (row,col) dir = 1000 * row + 4 * col + fromEnum dir

solve :: Mapping -> Correspondence -> ([Text], Text) -> Int
solve mapping correspondence = (password <$> both (+1) . loc <*> dir) . uncurry (act $ getMapping mapping correspondence) . bimap (start . parseMap) parseMoves

solution1 :: IO Int
solution1 = solve mapping correspondenceFlat <$> input
-- 80392

solution2 :: IO Int
solution2 = solve mapping correspondenceCube <$> input
-- 19534




inputTest :: IO ([Text],Text)
inputTest = ((,) <$> normalize . init . init <*> last) . lines <$> readFile "input/input22-2.txt"

solutionTest1 :: IO Int
solutionTest1 = solve mappingTest correspondenceTestFlat <$> inputTest

solutionTest2 :: IO Int
solutionTest2 = solve mappingTest correspondenceTestCube <$> inputTest

mappingTest :: Mapping
mappingTest 1  = [((row,8) ,L) | row <-         [0..3]]
mappingTest 21 = [((4,col) ,U) | col <- reverse [4..7]]
mappingTest 22 = [((4,col) ,U) | col <- reverse [0..3]]
mappingTest 3  = [((row,0) ,L) | row <-         [4..7]]
mappingTest 41 = [((7,col) ,D) | col <-         [0..3]]
mappingTest 42 = [((7,col) ,D) | col <-         [4..7]]
mappingTest 5  = [((row,8) ,L) | row <-         [8..11]]
mappingTest 61 = [((11,col),D) | col <-         [8..11]]
mappingTest 62 = [((11,col),D) | col <-         [12..15]]
mappingTest 7  = [((row,15),R) | row <- reverse [8..11]]
mappingTest 8  = [((8,col) ,U) | col <- reverse [12..15]]
mappingTest 91 = [((row,11),R) | row <- reverse [4..7]]
mappingTest 92 = [((row,11),R) | row <- reverse [0..3]]
mappingTest 10 = [((0,col) ,U) | col <- reverse [9..11]]
mappingTest _ = []

correspondenceTestFlat :: Correspondence
correspondenceTestFlat 1  = 92
correspondenceTestFlat 21 = 42
correspondenceTestFlat 22 = 41
correspondenceTestFlat 3  = 91
correspondenceTestFlat 41 = 22
correspondenceTestFlat 42 = 21
correspondenceTestFlat 5  = 7
correspondenceTestFlat 61 = 10
correspondenceTestFlat 62 = 8
correspondenceTestFlat 7  = 5
correspondenceTestFlat 8  = 62
correspondenceTestFlat 91 = 3
correspondenceTestFlat 92 = 1
correspondenceTestFlat 10 = 61

correspondenceTestCube :: Correspondence
correspondenceTestCube 1  = 21
correspondenceTestCube 21 = 1
correspondenceTestCube 22 = 10
correspondenceTestCube 3  = 62
correspondenceTestCube 41 = 61
correspondenceTestCube 42 = 5
correspondenceTestCube 5  = 42
correspondenceTestCube 61 = 41
correspondenceTestCube 62 = 3
correspondenceTestCube 7  = 92
correspondenceTestCube 8  = 91
correspondenceTestCube 91 = 8
correspondenceTestCube 92 = 7
correspondenceTestCube 10 = 22

