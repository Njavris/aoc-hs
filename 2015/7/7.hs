import System.IO
import qualified Data.Map.Lazy as Map
import Data.Word
import Data.Bits
import Text.Read

data Operand = Val Word16 | Wire String
  deriving (Show, Eq)
data Gate = NOT Operand | AND Operand Operand | OR Operand Operand |
  LSHIFT Operand Int | RSHIFT Operand Int | ID Operand
  deriving (Show, Eq)

parseOperand :: String -> Operand
parseOperand str = case readMaybe str :: Maybe Word16 of
  Just val -> Val val
  Nothing -> Wire str

parseLine :: String -> (String, Gate)
parseLine ln = case words ln of
  [val, "->", label] -> (label, (ID (parseOperand val)))
  ["NOT", op , "->", label] -> (label, (NOT (parseOperand op)))
  [op1, "AND", op2 , "->", label] -> (label, (AND (parseOperand op1) (parseOperand op2)))
  [op1, "OR", op2 , "->", label] -> (label, (OR (parseOperand op1) (parseOperand op2)))
  [op1, "LSHIFT", op2 , "->", label] -> (label, (LSHIFT (parseOperand op1) (read op2 :: Int)))
  [op1, "RSHIFT", op2 , "->", label] -> (label, (RSHIFT (parseOperand op1) (read op2 :: Int)))

evalGate :: Gate -> Map.Map String Word16 -> Word16
evalGate (ID op) wires = evalOperand op wires
evalGate (NOT op) wires = complement $ evalOperand op wires
evalGate (AND op1 op2) wires = (evalOperand op1 wires) .&. (evalOperand op2 wires)
evalGate (OR op1 op2) wires = (evalOperand op1 wires) .|. (evalOperand op2 wires)
evalGate (LSHIFT op shift) wires = shiftL (evalOperand op wires) shift
evalGate (RSHIFT op shift) wires = shiftR (evalOperand op wires) shift

evalOperand :: Operand -> Map.Map String Word16 -> Word16
evalOperand (Val w16) _ = w16
evalOperand (Wire label) wires = case Map.lookup label wires of
  Just v -> v
  Nothing -> error ("evalOperand: couldn't find " ++ show label)

evalAllGates :: Map.Map String Gate -> Map.Map String Word16
evalAllGates wires = lzy
  where lzy = Map.map (\g -> evalGate g lzy) wires

solve :: String -> Map.Map String Gate -> Word16
solve label wires = let lzy = evalAllGates wires
  in case Map.lookup label lzy of
    Just val -> val
    Nothing -> error ("Solve: couldn't find " ++ show label)

main :: IO ()
main = readFile "input.txt" >>= \input -> do
  let
    wiresPartOne = Map.fromList $ map parseLine $ lines input
    partOne = solve "a" wiresPartOne
    wiresPartTwo = Map.insert "b" (ID (Val partOne)) wiresPartOne
    partTwo = solve "a" wiresPartTwo

  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
