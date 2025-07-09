import Data.List
import Data.List.Split

getChars :: String -> [String]
getChars str = filter (/= "\n") $ splitOn "" str

data Position = Position {
    x :: Int,
    y :: Int
} deriving (Show, Eq)

charToDirection :: String -> Position
charToDirection "^" = Position { x = 0,  y = 1  }
charToDirection "v" = Position { x = 0,  y = -1 }
charToDirection ">" = Position { x = 1,  y = 0  }
charToDirection "<" = Position { x = -1, y = 0  }
charToDirection _   = Position { x = 0,  y = 0  }

travel :: Position -> Position -> Position
travel from by = Position {
    x = x from + x by,
    y = y from + y by
}

main :: IO ()
main = do
    contents <- readFile "data/Day3/input"

    let chars         = getChars contents
    let directions    = map charToDirection chars
    let indices1      = filter (\x -> x `mod` 2 == 0) [0..length directions - 1]
    let indices2      = filter (\x -> x `mod` 2 /= 0) [0..length directions - 1]
    let directions1   = [directions !! x | x <- indices1]
    let directions2   = [directions !! x | x <- indices2]
    let startingPoint = Position { x = 0, y = 0 }

    let travelHistory = nub $ (scanl travel startingPoint directions1) ++ (scanl travel startingPoint directions2)

    print $ length travelHistory
