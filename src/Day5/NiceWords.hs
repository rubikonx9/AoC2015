import Data.List

isStringNice :: String -> Bool
isStringNice string = has3Vovels string
                   && hasDuplicateLetter string
                   && doesntHaveForbidden string

has3Vovels :: String -> Bool
has3Vovels string = length vovels >= 3
    where vovels = filter (\x -> x `elem` "aeiou") string

hasDuplicateLetter :: String -> Bool
hasDuplicateLetter string = any (\substr -> length substr >= 2) $ group string

doesntHaveForbidden :: String -> Bool
doesntHaveForbidden string = not $ any (\forbidden -> forbidden `isInfixOf` string) forbiddens
    where forbiddens = ["ab", "cd", "pq", "xy"]

main :: IO ()
main = do
    contents <- readFile "data/Day5/input"

    let niceStrings = filter isStringNice $ lines contents

    print $ length niceStrings
