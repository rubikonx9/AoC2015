import Data.List
import Data.List.Split

parseBoxInfo :: String -> [Int]
parseBoxInfo str = map (read) $ splitOn "x" str

data Box = Box {
    len    :: Int,
    width  :: Int,
    height :: Int
} deriving (Show)

intsToBox :: [Int] -> Box
intsToBox []          = intsToBox [ 1, 1, 1 ]
intsToBox [ l ]       = intsToBox [ l, 1, 1 ]
intsToBox [ l, w ]    = intsToBox [ l, w, 1 ]
intsToBox [ l, w, h ] = Box { len = l, width = w, height = h }

shortestSides :: Box -> [Int]
shortestSides box = take 2 $ sort [ len box, width box, height box ]

wrappingLength :: Box -> Int
wrappingLength box = sum sides * 2 + volume
    where sides  = shortestSides box
          volume = len box * width box * height box

main :: IO ()
main = do
    contents <- readFile "data/Day2/input"
    let boxesSizes = map (parseBoxInfo) (lines contents)
    let boxes      = map (intsToBox) boxesSizes
    let boxesAreas = map (wrappingLength) boxes

    print $ sum boxesAreas
