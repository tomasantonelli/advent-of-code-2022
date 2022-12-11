import Control.Monad
import Data.Array.IArray
import Data.Char
import qualified Data.List as List

type Coord     = (Int, Int)
type HeightMap = Array Coord Int
type MapSize   = Coord
data Direction = DLeft | DRight | DUp | DDown deriving Eq

tallestHeightFrom :: HeightMap -> MapSize -> Direction -> Coord -> Int
tallestHeightFrom hm dims d (r, c)
    | d == DLeft  = if c == 1       then h else max h $ tallestHeightFrom hm dims d (r, c-1)
    | d == DRight = if c == numCols then h else max h $ tallestHeightFrom hm dims d (r, c+1)
    | d == DUp    = if r == 1       then h else max h $ tallestHeightFrom hm dims d (r-1, c)
    | d == DDown  = if r == numRows then h else max h $ tallestHeightFrom hm dims d (r+1, c)
    where h = hm ! (r, c)
          (numRows, numCols) = dims

visibleFrom :: HeightMap -> MapSize -> Direction -> Coord -> Bool
visibleFrom hm dims d (r, c)
    | d == DLeft  = if c ==       1 then True else h > tallestHeightFrom hm dims d (r, c-1)
    | d == DRight = if c == numCols then True else h > tallestHeightFrom hm dims d (r, c+1)
    | d == DUp    = if r ==       1 then True else h > tallestHeightFrom hm dims d (r-1, c)
    | d == DDown  = if r == numRows then True else h > tallestHeightFrom hm dims d (r+1, c)
    where h = hm ! (r, c)
          (numRows, numCols) = dims

visible :: HeightMap -> MapSize -> Coord -> Bool
visible hm dims pos =
    visibleFrom hm dims DLeft  pos ||
    visibleFrom hm dims DRight pos ||
    visibleFrom hm dims DUp    pos ||
    visibleFrom hm dims DDown  pos

maxReachFrom :: HeightMap -> MapSize -> Direction -> Coord -> Int
maxReachFrom hm dims d pos = solve hm dims d pos initialHeight
    where initialHeight = hm ! pos
          solve hm dims d (r, c) h
            | r == 1 || r == numRows || c == 1 || c == numCols = 0
            | d == DLeft  = if h <= hm ! (r, c-1) then 1 else 1 + solve hm dims d (r, c-1) h
            | d == DRight = if h <= hm ! (r, c+1) then 1 else 1 + solve hm dims d (r, c+1) h
            | d == DUp    = if h <= hm ! (r-1, c) then 1 else 1 + solve hm dims d (r-1, c) h
            | d == DDown  = if h <= hm ! (r+1, c) then 1 else 1 + solve hm dims d (r+1, c) h
            where (numRows, numCols) = dims

scenicScore :: HeightMap -> MapSize -> Coord -> Int
scenicScore hm dims pos =
    (maxReachFrom hm dims DLeft  pos) *
    (maxReachFrom hm dims DRight pos) *
    (maxReachFrom hm dims DUp    pos) *
    (maxReachFrom hm dims DDown  pos)

main = do 
    input <- readFile "./inputs/main"
    let heightsList  = map (map (\c -> (ord c) - (ord '0'))) (lines input)
        numRows      = length heightsList
        numCols      = length $ heightsList !! 0
        dimensions   = (numRows, numCols)
        heights      = listArray ((1, 1), dimensions) (join heightsList) :: HeightMap
        positions    = [(i,j) | i <- [1..numRows], j <- [1..numCols]]
    let visibles     = map (visible heights dimensions) positions
        scenicScores = map (scenicScore heights dimensions) positions
    putStrLn $ "Part One: " ++ show (sum $ map fromEnum visibles)
    putStrLn $ "Part Two: " ++ show (foldl max 0 scenicScores)