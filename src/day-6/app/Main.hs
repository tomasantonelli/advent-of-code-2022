import Data.MultiSet (MultiSet, delete, fromList, insert, distinctSize)

main = do
  input <- readFile "./inputs/main"
  let inputLines = lines input
  let resultLines = map (show . findFirstWindowWithMarker) inputLines
  mapM_ putStrLn resultLines

data Window =
  Window { start :: Int, end :: Int, bag :: MultiSet Char, sourceString :: String }

instance Show Window where show w = show (end w)

initializeWindow :: String -> Window
initializeWindow s = Window
  { start = 0, end = 14, bag = fromList firstChars, sourceString = s }
  where firstChars = take 14 s

windowIsAtTheEnd :: Window -> Bool
windowIsAtTheEnd w = (end w) >= length (sourceString w)

slideWindow :: Window -> Window
slideWindow w =
  let s = sourceString w
      i = start w
      j = end w
      b = bag w
   in if windowIsAtTheEnd w
        then w
        else let updateBag = (delete charToBeRemoved) . (insert charToBeAdded)
                 charToBeRemoved = s !! i
                 charToBeAdded = s !! j
              in Window
                   { start = i + 1
                   , end = j + 1
                   , bag = updateBag b
                   , sourceString = s
                   }

windowHasMarker :: Window -> Bool
windowHasMarker w = (distinctSize $ bag w) == end w - start w

findFirstWindowWithMarker :: String -> Window
findFirstWindowWithMarker s = searchFrom (initializeWindow s)
  where searchFrom :: Window -> Window
        searchFrom w =
          if (windowHasMarker w) || (windowIsAtTheEnd w)
          then w
          else searchFrom (slideWindow w)