import Data.List
import Data.List.Split

getChars :: String -> [String]
getChars str = filter (/= "\n") $ splitOn "" str

parseFloorChar :: String -> Int
parseFloorChar "(" = 1
parseFloorChar ")" = -1
parseFloorChar _   = 0

main = do
    contents <- readFile "input"
    let chars      = getChars contents
    let ints       = filter (/= 0) $ map parseFloorChar chars
    let conseqSums = tail $ scanl (+) 0 ints

    print $ ((\(Just i) -> i) $ findIndex (== -1) conseqSums) + 1
