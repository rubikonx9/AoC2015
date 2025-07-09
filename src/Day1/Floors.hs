import Data.List.Split

getChars :: String -> [String]
getChars str = filter (/= "\n") $ splitOn "" str

parseFloorChar :: String -> Int
parseFloorChar "(" = 1
parseFloorChar ")" = -1
parseFloorChar _   = 0

main :: IO ()
main = do
    contents <- readFile "data/Day1/input"
    let chars = getChars contents
    let ints  = map parseFloorChar chars

    print $ sum ints
