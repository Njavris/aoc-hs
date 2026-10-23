import System.IO
import System.Environment
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (transpose)

data ISA = RECT (Int, Int) | ROR (Int, Int) | ROC (Int, Int) deriving (Show)

parseInstr :: String -> ISA
parseInstr = go . words
  where
    parsePair :: [String] -> (Int, Int)
    parsePair (dim:"by":n:_) = case break (== '=') dim of
                               (_, '=' :v) -> (read v, read n)
                               e -> error $ ("Error parsing: " ++) $ show e

    go :: [String] -> ISA
    go ("rect":dims:_) = RECT $ case break (== 'x') dims of
                                (x, 'x' :y) -> (read x, read y)
                                e -> error $ ("Error parsing: " ++) $ show e
    go ("rotate":"row":rest) = ROR $ parsePair rest
    go ("rotate":"column":rest) = ROC $ parsePair rest
    go v = error $ ("Err: Unknown instruction: " ++) $ show v

type Screen = [[Char]]

rotateRow :: Int -> Int -> Screen -> Screen
rotateRow r n scr = [if id == r then rotate n row else row | (id, row) <- zip [0 ..] scr]
  where
    rotate n row =
      let
        l = length row
        pre = (l - (n `mod` l)) `mod` l
      in
        take l $ drop pre $ cycle row

process :: Screen -> ISA -> Screen
process scr (RECT (x, y)) =
  [[ if x' < x && y' < y then '#' else orig
    | (x', orig) <- zip [0 ..] row]
      | (y', row) <- zip [0 ..] scr]
process scr (ROR (y, n)) = rotateRow y n scr
process scr (ROC (x, n)) = transpose (rotateRow x n (transpose scr))


main :: IO ()
main = do
  input <- readFile . fromMaybe "input.txt" . listToMaybe =<< getArgs
  let
    instrs = reverse $ map parseInstr $ lines input
    screen = [['.' | x <- [0 .. 49]] | y <- [0 .. 5]]
    result = foldr (\instr scr -> process scr instr) screen instrs
    partOne = length $ filter (== '#') $ concat result
    splitLine = (\s -> concat [ if i `mod` 5 == 0
                                then ['|', x]
                                else [x] | (i, x) <- zip [0..] s])
    partTwo = map splitLine result

  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn "Part Two:"
  mapM_ putStrLn $ partTwo
