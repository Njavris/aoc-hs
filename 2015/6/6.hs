import System.IO
import Data.List

type Point = (Int, Int)
type Rect  = (Point, Point)

data Command = TurnOn | TurnOff | Toggle
data Instruction = Inst Command Rect

class ApplyCmd a where
  doit:: Command -> a -> a

insideRect :: Point -> Rect -> Bool
insideRect (x, y) ((x0, y0), (x1, y1)) =
  x >= x0 &&
  x <= x1 &&
  y >= y0 &&
  y <= y1

evalInst :: ApplyCmd a => Point -> a -> Instruction -> a
evalInst pt curr (Inst cmd rect)
  | insideRect pt rect = doit cmd curr
  | otherwise          = curr

evalPoint :: ApplyCmd a => a -> [Instruction] -> Point -> a
evalPoint init insts pt = foldl' (evalInst pt) init insts

points :: [Point]
points = [(x, y) | x <- [0 .. 999], y <- [0 .. 999]]

-- Part One:
instance ApplyCmd Bool where
  doit TurnOn _ = True
  doit TurnOff _ = False
  doit Toggle curr = not curr

partOne :: [Instruction] -> Int
partOne insts = length $ filter id $ map (evalPoint False insts) points

-- Part Two:
instance ApplyCmd Int where
  doit TurnOn curr = curr + 1
  doit TurnOff curr = max 0 (curr - 1)
  doit Toggle curr = curr + 2

partTwo :: [Instruction] -> Int
partTwo insts = sum $ map (evalPoint 0 insts) points


parseLine :: String -> Instruction
parseLine ln
  | Just rest <- stripPrefix "turn on " ln = 
    let [x1, y1, _, x2, y2] = words (map (\v -> if v == ',' then ' ' else v) rest)
    in Inst TurnOn ((read x1, read y1), (read x2, read y2))
  | Just rest <- stripPrefix "turn off " ln =
    let [x1, y1, _, x2, y2] = words (map (\v -> if v == ',' then ' ' else v) rest)
    in Inst TurnOff ((read x1, read y1), (read x2, read y2))
  | Just rest <- stripPrefix "toggle " ln =
    let [x1, y1, _, x2, y2] = words (map (\v -> if v == ',' then ' ' else v) rest)
    in Inst Toggle ((read x1, read y1), (read x2, read y2))

main :: IO ()
main = readFile "input.txt" >>= \input -> do
  let
    insts = map parseLine $ lines input
  putStrLn $ ("Part One: " ++) $ show (partOne insts)
  putStrLn $ ("Part Two: " ++) $ show (partTwo insts)

