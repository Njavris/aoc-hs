import System.IO
import System.Environment
import Data.List (sort)

parseInput :: [String] -> [[Int]]
parseInput = map (map (read :: String -> Int) . words)

transpose :: [[Int]] -> [[Int]]
transpose [] = []
transpose (a:b:c:rest) = go [a, b, c] ++ transpose rest
  where go [[a1, b1, c1],[a2, b2, c2],[a3, b3, c3]] =
                [[a1, a2, a3],[b1, b2, b3],[c1, c2, c3]]
transpose v = error $ ("error: " ++) $ show v

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ case args of
                      (v:_) -> v
                      _ -> "input.txt"
  let
    sides = parseInput $ lines input
    muhSort = (\lst -> case sort lst of
                           [a, b, c] -> (a, b, c)
                           v -> error $ ("error: " ++) $ show v)
    countValid = (\lst -> length [(a, b, c) | (a, b, c) <- lst, a + b > c])
    solve = (\f -> countValid . (map muhSort) . f)
    partOne = solve id sides
    partTwo = solve transpose sides

  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo

