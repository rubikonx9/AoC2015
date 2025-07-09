import Data.List

isStringNice :: String -> Bool
isStringNice string = hasNiceDuplicate string
                   && hasPair string

isLength3 :: String -> Bool
isLength3 string = length string == 3

isNiceDuplicate :: String -> Bool
isNiceDuplicate string = string !! 0 == string !! 2

hasNiceDuplicate :: String -> Bool
hasNiceDuplicate string = any isNiceDuplicate realSubseqs3
    where realSubseqs3 = filter (\x -> x `isInfixOf` string) subseqs3
          subseqs3     = filter isLength3 subseqs
          subseqs      = subsequences string

main :: IO ()
main = do
    contents <- readFile "data/Day5/input"

    let niceStrings = filter isStringNice $ lines contents
    print $ length niceStrings

hasPair :: String -> Bool
hasPair string = any (== True) checks
    where checks = [ word2 `elem` subsequentSubstrings | x <- [0..length substrings - 1],
              let subsequentSubstrings = drop (x + 2) substrings,
              let word2 = (take 1 $ drop x substrings) !! 0 ]
          substrings = len2Substrings string

len2Substrings :: String -> [String]
len2Substrings string = [ [ string !! x, string !! (x + 1) ] | x <- [0..length string - 2] ]
