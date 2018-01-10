import Data.List
import Data.List.Split

getChars :: String -> [String]
getChars str = filter (/= "\n") $ splitOn "" str

data Position = Position {
    x :: Int,
    y :: Int
} deriving (Show, Ord, Eq)

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

main = do
    contents <- readFile "input"

    let chars         = getChars contents
    let directions    = map charToDirection chars
    let startingPoint = Position { x = 0, y = 0 }
    let travelHistory = nub $ scanl travel startingPoint directions

    print $ length travelHistory
