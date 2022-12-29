{-# LANGUAGE OverloadedStrings, TupleSections, NamedFieldPuns, RecordWildCards #-}
module Day17 where
import Universum hiding (replicate, last, State, take, drop, head, tail, minimum, splitAt, maximum, init, drop, take)
import Data.Text as T (Text, unpack, index, singleton, splitAt)
import qualified Data.Text as T
import Data.Sequence as S (replicate, findIndexR, adjust', (><), dropWhileR, Seq, takeWhileR)
import qualified Data.Sequence as S (index, drop)
import Data.List as L (take, last, head, tail, iterate', minimum, maximum, splitAt, init, scanl', (!!))
import Data.List.Extra (nubOrd, groupOn, chunksOf)
import Data.Tuple.Extra (both)
import Data.Foldable.Extra (sumOn')
import Data.Maybe (fromJust)

input :: IO [Char]
input = unpack <$> readFile "input/input17.txt"

type Coord = (Int,Int)
type Shape = [Coord]
type Chamber = Seq Text

shapes :: [Shape]
shapes = [
    [(0,0),(0,1),(0,2),(0,3)],
    [(0,1),(1,0),(1,1),(1,2),(2,1)],
    [(0,0),(0,1),(0,2),(1,2),(2,2)],
    [(0,0),(1,0),(2,0),(3,0)],
    [(0,0),(0,1),(1,0),(1,1)]
  ]

newRow :: Text
newRow = "......."

shapeHeight :: Shape -> Int
shapeHeight = (+1) . maximum . fmap fst

data State = State {
    jet         :: [Char],
    chamber     :: Chamber,
    falling     :: Maybe Shape,
    shapeStream :: [Shape],
    phase       :: Bool,
    fixed       :: Int,
    pruned      :: Int
}

initialState :: [Char] -> State
initialState jet = State jetStream chamber (Just firstShape) (tail shapeStream) True 0 0
    where chamber     = replicate ((+3) $ shapeHeight $ head shapeStream) newRow
          shapeStream = cycle shapes
          jetStream   = cycle jet
          firstShape  = move (appearance chamber) (head shapeStream)

appearance :: Chamber -> Coord
appearance = (,2) . maybe 3 (+4) . findIndexR (/= newRow)

isFree :: Coord -> Chamber -> Bool
isFree (row,col) c | row < 0         || col < 0  = False
isFree (row,col) c | row >= length c || col >= 7 = False
isFree (row,col) c                               = (== '.') . (`index` col) . (`S.index` row) $ c

canFit :: Chamber -> Shape -> Bool
canFit chamber = all (\(x,y) -> isFree (x,y) chamber)

move :: Coord -> Shape -> Shape
move (dx,dy) = fmap (\(x,y) -> (x+dx,y+dy))

fixShape :: Char -> Chamber -> Shape -> Chamber
fixShape c chamber = foldl' (\cham rowcol -> adjust' (\rr -> foldl' (\r col -> let (start,end) = T.splitAt col r in start <> singleton c <> T.tail end) rr $ fmap snd rowcol) (fst $ head rowcol) cham) chamber . groupOn fst

rowsToPrune :: Chamber -> Int
rowsToPrune c = minimum $ fmap (\i -> fromMaybe 0 $ findIndexR ((== '#') . (`index` i)) c) [0..length (c `S.index` 0)-1]

pruneState :: State -> State
pruneState (State jet chamber falling shapeStream phase fixed pruned) = State jet (toPrune `S.drop` chamber) (fmap (first (flip (-) toPrune)) <$> falling) shapeStream phase fixed (pruned + toPrune)
    where toPrune = rowsToPrune chamber

jetDirection :: [Char] -> Int
jetDirection ('<':_) = -1
jetDirection ('>':_) = 1

prependTo :: Int -> Chamber -> Chamber
prependTo rows c = (c ><) . (`replicate` newRow) . max 0 . (rows -) . length . takeWhileR (== newRow) $ c

act :: Bool -> State -> State
act canPrune state@State{chamber} | canPrune && length chamber > 1000 = if canPrune then pruneState state else state
act _  (State jet chamber Nothing     s phase f p) = let newChamber = prependTo ((+3) $ shapeHeight $ head s) chamber
                                                         newFalling = move (appearance newChamber) (head s)
                                                      in State jet newChamber (Just newFalling) (tail s) phase f p
act _ (State jet chamber (Just shape) s True  f p) = let moved = move (0,jetDirection jet) shape
                                                         newFalling = if canFit chamber moved then moved else shape
                                                     in State (tail jet) chamber (Just newFalling) s False f p
act _ (State jet chamber (Just shape) s False f p) = let moved = move (-1,0) shape
                                                     in if canFit chamber moved
                                                        then State jet chamber                     (Just moved) s True f     p
                                                        else State jet (fixShape '#' chamber shape) Nothing     s True (f+1) p

solve :: Int -> [Char] -> Int
solve rocks = ((+) <$> length . dropWhileR (== newRow) . chamber <*> pruned) . head . dropWhile ((< rocks) . fixed) . iterate' (act True) . initialState

solution1 :: IO Int
solution1 = solve 2022 <$> input
-- 3100

split :: (Int, Int) -> Chamber -> ([Text], [[Text]])
split (prefix,chunk) = second (init . chunksOf chunk) . L.splitAt prefix . toList

splitCandidates :: Chamber -> [(Int, Int)]
splitCandidates c = [(prefixSize,chunkSize) | prefixSize <- [0..(length c `div` 2)],
                                              chunkSize  <- [(max prefixSize 4)..(length c `div` 2)],
                                              prefixSize == 0 || chunkSize `mod` prefixSize /= 0,
                                              prefixSize + chunkSize*2 <= length c]

findPrefixAndRepeating :: Chamber -> [([Text],[Text])]
findPrefixAndRepeating = fmap (second head) . filter (null . tail . nubOrd . snd) . filter ((>=2) . length . snd) . (fmap <$> flip split <*> splitCandidates)

solvePrefixAndRepeating :: [Char] -> [([Text], [Text])]
solvePrefixAndRepeating = concatMap (findPrefixAndRepeating . chamber . snd) . filter ((==0) . (`mod` 10000) . fst) . zip [0..] . iterate' (act False) . initialState

rocksIn :: [Text] -> Int
rocksIn c = length $ takeWhile (<= amountOfRock) $ tail $ scanl' (+) 0 shapeSizes
    where amountOfRock = sumOn' (T.foldl' (\a ch -> a + if ch == '#' then 1 else 0) 0 ) c
          shapeSizes = cycle $ fmap length shapes

calculateHeights :: [((Int, Int), (Int, Int))] -> Int
calculateHeights a = uncurry calculateHeight . fromJust $ find (\((_,prefixRocks),(_,repeatingRocks)) -> (1000000000000 - prefixRocks) `mod` repeatingRocks == 0) a

calculateHeight :: (Int, Int) -> (Int, Int) -> Int
calculateHeight (prefixLength,prefixRocks) (repeatingLength,repeatingRocks) = prefixLength + repeatsNeeded * repeatingLength
    where (repeatsNeeded,remainingRocks) = (1000000000000 - prefixRocks) `divMod` repeatingRocks

solution2 :: IO Int
solution2 = calculateHeights . fmap (both ((,) <$> length <*> rocksIn)) . solvePrefixAndRepeating <$> input
-- 1540634005751

simulate :: Int -> IO ()
simulate i = ((draw . (!! i) . iterate' (act False) . initialState) . unpack =<< readFile "input/input17.txt") >> putTextLn ""
    where draw :: State -> IO ()
          draw State{..} = drawChamber $ maybe chamber (fixShape '@' chamber) falling
          drawChamber c = do
            traverse_ (putStrLn . toList) . reverse . toList $ c
            putTextLn "--- repeating: ---"
            case findPrefixAndRepeating c of
                [(prefix,x)] -> do
                    print "----- Prefix: "
                    traverse_ (putStrLn . toList) . reverse . fromList $ prefix
                    print "----- Repeating: "
                    traverse_ (putStrLn . toList) . reverse . fromList $ x
                    print $ "Prefix length: " <> show (length prefix) <> ", repeating length: " <> show (length x)
                    print $ "----- rocks in: " <> show (rocksIn prefix) <> ", " <> show (rocksIn x)
                [] -> putTextLn "-"
            pure ()