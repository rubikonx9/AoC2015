import Data.Bits
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Word
import Text.Read

data Operation = NOP | ASSIGN | AND | OR | LSHIFT | RSHIFT | NOT
    deriving (Show, Eq)

data Instruction = Instruction {
    operation   :: Operation,
    outputName  :: String,
    inputsNames :: [String]
} deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction rawInstruction = Instruction { operation = operation, inputsNames = inputsNames, outputName = outputName }
    where inputParts               = splitOn " " inputs
          (inputs, outputName)     = parse rawInstruction
          (operation, inputsNames) = if length inputParts == 1        then (ASSIGN, [inputParts !! 0])
                                else if (inputParts !! 0) == "NOT"    then (NOT,    [inputParts !! 1])
                                else if (inputParts !! 1) == "AND"    then (AND,    [inputParts !! 0, inputParts !! 2])
                                else if (inputParts !! 1) == "OR"     then (OR,     [inputParts !! 0, inputParts !! 2])
                                else if (inputParts !! 1) == "LSHIFT" then (LSHIFT, [inputParts !! 0, inputParts !! 2])
                                else if (inputParts !! 1) == "RSHIFT" then (RSHIFT, [inputParts !! 0, inputParts !! 2])
                                else (NOP, [])

getInstruction :: String -> [Instruction] -> Instruction
getInstruction instructionName allInstructions = fromMaybe dum $ find (\x -> outputName x == instructionName) allInstructions
    where dum = Instruction {  operation = NOP, inputsNames = [], outputName = ""  }

execByNames :: [String] -> [Instruction] -> [Word16]
execByNames instructionsNames allInstructions = map (\x -> execByName x allInstructions) instructionsNames

execByName :: String -> [Instruction] -> Word16
execByName instructionName allInstructions = output
    where output            = exec sourceInstruction allInstructions
          sourceInstruction = getInstruction (instructionName) allInstructions

exec :: Instruction -> [Instruction] -> Word16
exec instruction allInstructions = result
    where op     = operation instruction
          result = if op == ASSIGN then execAssign instruction allInstructions
              else if op == NOT    then execNot    instruction allInstructions
              else if op == AND    then execAnd    instruction allInstructions
              else if op == OR     then execOr     instruction allInstructions
              else if op == LSHIFT then execLshift instruction allInstructions
              else if op == RSHIFT then execRshift instruction allInstructions
              else 0

execAssign :: Instruction -> [Instruction] -> Word16
execAssign instruction allInstructions = fromMaybe source maybeResult
    where maybeResult       = readMaybe inputName
          inputName         = inputsNames instruction !! 0
          source            = execByName inputName allInstructions

execNot :: Instruction -> [Instruction] -> Word16
execNot instruction allInstructions = complement (sources !! 0)
    where sources = execByNames (inputsNames instruction) allInstructions

execAnd :: Instruction -> [Instruction] -> Word16
execAnd instruction allInstructions = (fromMaybe sourceA maybeNumber) .&. sourceB
    where maybeNumber = readMaybe inputNameA
          inputNameA  = inputsNames instruction !! 0
          inputNameB  = inputsNames instruction !! 1
          sourceA     = execByName inputNameA allInstructions
          sourceB     = execByName inputNameB allInstructions

execOr :: Instruction -> [Instruction] -> Word16
execOr instruction allInstructions = (sources !! 0) .|. (sources !! 1)
    where sources = execByNames (inputsNames instruction) allInstructions

execLshift :: Instruction -> [Instruction] -> Word16
execLshift instruction allInstructions = source `shiftL` offset
    where source = execByName ((inputsNames instruction) !! 0) allInstructions
          offset = read ((inputsNames instruction) !! 1)

execRshift :: Instruction -> [Instruction] -> Word16
execRshift instruction allInstructions = source `shiftR` offset
    where source = execByName ((inputsNames instruction) !! 0) allInstructions
          offset = read ((inputsNames instruction) !! 1)

parse :: String -> (String, String)
parse str = (input, output)
    where parts  = splitOn " -> " str
          output = parts !! 1
          input  = parts !! 0

notEmpty :: String -> Bool
notEmpty str = not $ null str

main :: IO ()
main = do
    contents <- readFile "data/Day7/input"

    let rawInstructions = filter notEmpty (lines contents)
    let allInstructions = map parseInstruction rawInstructions

    print $ execByName "a" allInstructions
