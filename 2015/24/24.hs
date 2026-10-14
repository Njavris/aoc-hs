import System.IO
import System.Environment
import Data.List (sort, group, nub, (\\))

parseInput :: String -> [Int]
parseInput input = [(read ln :: Int) | ln <- lines input]

fillPartition :: [Int] -> [Int] -> Int -> [[Int]]
fillPartition _ [] _ = []
fillPartition part (r:remaining) target
  | sumNewPart == target = [newPart] ++ fillPartition part remaining target
  | sumNewPart < target = fillPartition newPart remaining target ++ fillPartition part remaining target
  | otherwise = fillPartition part remaining target
  where
    newPart = (r:part)
    sumNewPart = sum newPart

calcQE :: [Int] -> (Int, Int)
calcQE partition = (length partition, product partition)

canFill :: [Int] -> Int -> [Int] -> Bool
canFill weights target part =
  let filteredWeights = filter (`notElem` part) weights
  in not . null $ fillPartition [] filteredWeights target

main :: IO ()
main = do
  args <- getArgs
  let fn = case args of
           (v:_) -> v
           _ -> "input.txt"
  input <- readFile fn
  let
    weights = parseInput input
    target = div $ sum weights
    parts = fillPartition [] weights . target
    validParts = (filter . canFill weights . target) <*> parts
    answer = show . snd . head . sort . map calcQE . validParts

  putStrLn $ ("Part One: " ++) $ answer 3
  putStrLn $ ("Part Two: " ++) $ answer 4

