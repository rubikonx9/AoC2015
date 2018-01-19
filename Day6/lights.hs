import Data.List.Split
import Data.List
import Data.Array

-- This solution seems correct, but doesnt work due to extreme memory consumption.

data Action = TurnOn | TurnOff | Toggle deriving (Show)

data Instruction = Instruction {
    position :: (Int, Int),
    action   :: Action
} deriving (Show)

action2value :: Action -> Int -> Int
action2value TurnOn  _ = 1
action2value TurnOff _ = 0
action2value Toggle  b = case b of 0 -> 1
                                   1 -> 0

str2action :: String -> Action
str2action str = if "turn on" `isPrefixOf` str then TurnOn
                 else if "turn off" `isPrefixOf` str then TurnOff
                 else Toggle

str2range :: String -> [ (Int, Int) ]
str2range str = range (start, end)
    where wrds  = reverse $ words $ str
          start = str2ints $ wrds !! 2
          end   = str2ints $ wrds !! 0

str2ints :: String -> (Int, Int)
str2ints str = (read x, read y)
    where [x, y] = splitOn delim str
          delim  = ","

str2instructions :: String -> [ Instruction ]
str2instructions str = [ Instruction { position = pos, action = act } | pos <- poss ]
    where poss = str2range str
          act  = str2action str

updateGrid :: Array (Int, Int) Int -> Instruction -> Array (Int, Int) Int
updateGrid arr instr = arr // [ (pos, val) ]
    where pos = position instr
          val = action2value (action instr) (arr ! pos)

main = do
    contents <- readFile "input"

    let instructions = intercalate [] $ map str2instructions (lines contents)
    let grid = array ((0,0),(999,999)) [ ((x, y), 0) | y <- [0..999], x <- [0..999] ]

    let gridOut = foldl updateGrid grid instructions

    let suma = foldl (+) 0 gridOut

    print $ suma

