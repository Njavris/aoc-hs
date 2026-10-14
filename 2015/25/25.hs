import System.IO
import System.Environment
import Data.Char (isDigit)

parseInput :: String -> (Int, Int)
parseInput input =
  case map read (words (map (\a -> if isDigit a then a else ' ') input)) of
    [r, c] -> (r, c)
    _ -> error "parseInput fail"

solve :: (Int, Int) -> Int
solve (row, col) =
  let
    rowStart :: Int -> Int
    rowStart 1 = 1
    rowStart i = (rowStart (i - 1)) + i - 1

    code :: Int -> Int
    code 1 = 20151125
    code n = (code (n - 1) * 252533) `rem` 33554393

    idx = rowStart (row + col - 1) + col - 1
  in
    code idx

main :: IO()
main = do
  args <- getArgs
  let fn = case args of
           (v:_) -> v
           _ -> "input.txt"
  input <- readFile fn

  let
    partOne = solve $ parseInput input

  putStrLn $ ("Part One: " ++) $ show partOne
