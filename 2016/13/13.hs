import System.IO
import System.Environment
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Bits (popCount)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.Sequence as Seq (Seq(..), singleton, (<|), (|>))

type Coord = (Int, Int)

calcNode :: Coord -> Int -> Bool
calcNode (x, y) seed =
  let
    n = x ^ 2 + 3 * x + 2 * x * y + y + y * y + seed
    setBits = popCount n
  in even setBits

calcNeighbors :: Int -> Coord -> [Coord]
calcNeighbors seed (x0, y0) =
  let
    dirs = [(0, -1), (0, 1), (-1, 0), (1, 0)]
    neighbors = map (\(dx, dy) -> (x0 + dx, y0 + dy)) dirs
    valid = (\p@(x, y) -> x >= 0 && y >= 0 && calcNode p seed)
  in filter valid neighbors


solve :: Coord -> Coord -> Int -> (Int, Int)
solve start end seed = go s m
  where
    s = Seq.singleton (start, 0)
    m = Map.singleton start 0 :: Map Coord Int
    go Seq.Empty _ = error "unexpected"
    go ((pos, steps) :<| rest) visited
     | pos == end = (steps, length $ filter (\(_, v) -> v < 50 ) $ Map.toList visited)
     | otherwise =
      let
        steps' = steps + 1
        nbrs = filter (\n -> Map.notMember n visited) $ calcNeighbors seed pos
        queue' = foldl (\q n -> q |> (n, steps')) rest nbrs
        visited' = foldl (\v n -> Map.insert n steps' v) visited nbrs
      in go queue' visited'

main :: IO ()
main = do
  input <- getArgs >>= \args -> return $ fromMaybe "10" (listToMaybe args)
  let
    seed = (read input) :: Int
    start = (0, 0)
    dest = (31, 39)
    (partOne, partTwo) = solve start dest seed
  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
