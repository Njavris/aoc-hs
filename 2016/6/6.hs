import System.IO
import System.Environment
import Data.List (transpose, group, sort, sortOn)

main :: IO ()
main = do
  input <- getArgs >>= (\args -> readFile $ case args of
                                            (fn:_) -> fn
                                            _ -> "input.txt")

  let
    transposed = transpose $ lines input
    grouped = map (group . sort) transposed
    flatten = \grp -> zip (map head grp) (map length grp)
    flattened = map flatten grouped
    sorted = map (sortOn (\(a, b) -> (negate b, a))) flattened

    partOne = map (fst . head) sorted
    partTwo = map (fst . last) sorted

  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
