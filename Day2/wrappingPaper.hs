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

sidesAreas :: Box -> [Int]
sidesAreas box = [ len box * width box, len box * height box, width box * height box ]

wrapingArea :: Box -> Int
wrapingArea box = sum areas * 2 + minimum areas
    where areas = sidesAreas box

main = do
    contents <- readFile "input"
    let boxesSizes = map (parseBoxInfo) (lines contents)
    let boxes      = map (intsToBox) boxesSizes
    let boxesAreas = map (wrapingArea) boxes

    print $ sum boxesAreas
