{-# LANGUAGE ViewPatterns #-}

import Data.Function
import Data.Maybe
import Data.Foldable (foldMap)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

data ShellLine = ShellCd String | ShellLs | ShellDir String | ShellFile Int String
    deriving Show

type ElfFilePath = [String]

type ElfFileSize = Int

data ElfFile = ElfFile { size :: Int, path :: ElfFilePath }

instance Show ElfFile where show f = show (path f) ++ " " ++ show (size f)

updateDirectory :: String -> ElfFilePath -> ElfFilePath
updateDirectory ".." = tail
updateDirectory dirName = (++) [dirName]

traverseFilesystem :: [ShellLine] -> ElfFilePath -> Map ElfFilePath ElfFileSize
traverseFilesystem (ShellCd   d   : sl) dir = traverseFilesystem sl $ updateDirectory d dir
traverseFilesystem (ShellFile s n : sl) dir = Map.insert (updateDirectory n dir) s $
    traverseFilesystem sl dir
traverseFilesystem [] _ = Map.empty
traverseFilesystem (ShellLs    : sl) dir = traverseFilesystem sl dir
traverseFilesystem (ShellDir _ : sl) dir = traverseFilesystem sl dir

getDirectorySizes :: [ShellLine] -> Map ElfFilePath ElfFileSize
getDirectorySizes sl =
    let paths = Map.toList $ traverseFilesystem sl []
    in  Map.fromListWith (+) $ foldMap dirSizesPerFile paths
    where dirSizesPerFile (p, s) = map
            (\dirPath -> (dirPath, s)) $
            (init . drop 1 . List.tails) p

doPartOne :: Map ElfFilePath ElfFileSize -> IO ()
doPartOne dirSizes = do
    let answer = Map.elems dirSizes & filter (<= 100000) & sum
    putStrLn $ "Part One: " ++ show answer

doPartTwo :: Map ElfFilePath ElfFileSize -> IO ()
doPartTwo dirSizes = do
    case Map.lookup ["/"] dirSizes of
        Nothing -> error "Uh oh!"
        Just usedSpace -> do
            let mustDelete = usedSpace - (70000000 - 30000000)
                largeEnoughFolders = Map.filter (>= mustDelete) dirSizes
                answer = largeEnoughFolders & Map.elems & List.sort & head
            putStrLn $ "Part Two: " ++ show answer

parseLine :: String -> Maybe ShellLine
parseLine "$ ls" = Just ShellLs
parseLine (List.stripPrefix "$ cd " -> Just dirName) = Just (ShellCd dirName)
parseLine (List.stripPrefix "dir " -> Just dirName) = Just (ShellDir dirName)
parseLine fileString = case words fileString of
    fileSize : fileName : _ -> Just (ShellFile (read fileSize) fileName)
    _ -> Nothing

main :: IO ()
main = do
    input <- readFile "./inputs/main"
    let parseResult = sequence $ map parseLine $ lines input
    case parseResult of
        Nothing -> putStrLn "Error parsing input"
        Just shellLines -> do
            let dirSizes = getDirectorySizes shellLines
            doPartOne dirSizes
            doPartTwo dirSizes