import Data.List.Split
import Data.List
import Data.Array.Unboxed

data Action = TurnOn | TurnOff | Toggle deriving (Show)

data Instruction = Instruction {
    action        :: Action,
    startPosition :: (Int, Int),
    endPosition   :: (Int, Int)
} deriving (Show)

type Grid = UArray (Int, Int) Int

parseInstruction :: String -> Instruction
parseInstruction str =
    let ws = words str
        (action, rest) = case ws of
            ("turn":"on":xs)  -> (TurnOn, xs)
            ("turn":"off":xs) -> (TurnOff, xs)
            ("toggle":xs)     -> (Toggle, xs)
        [x1, y1] = map read $ splitOn "," (rest !! 0)
        [x2, y2] = map read $ splitOn "," (rest !! 2)
    in Instruction action (x1, y1) (x2, y2)

initialGrid :: Grid
initialGrid = array ((0,0), (999,999)) [ ((x,y), 0) | x <- [0..999], y <- [0..999] ]

applyInstruction :: Grid -> Instruction -> Grid
applyInstruction grid (Instruction action (x1, y1) (x2, y2)) =
    grid // updates
  where
    updates = [ ((x, y), newValue (grid ! (x, y))) | x <- [x1..x2], y <- [y1..y2] ]
    newValue v = case action of
        TurnOn  -> 1
        TurnOff -> 0
        Toggle  -> 1 - v

main :: IO ()
main = do
    contents <- readFile "data/Day6/input"

    let instructions = map parseInstruction (lines contents)
    let finalGrid = foldl' applyInstruction initialGrid instructions
    print $ sum (elems finalGrid)
