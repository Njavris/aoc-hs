import System.IO
import System.Environment

decodeNumber :: [String] -> (Int, Int) -> Char
decodeNumber pad (x, y) = pad !! y !! x

move :: [String] -> Char -> (Int, Int) -> (Int, Int)
move pad instr pos =
  let
    m = case instr of
      'U' -> (0, -1)
      'D' -> (0, 1)
      'L' -> (-1, 0)
      'R' -> (1, 0)
      _ -> error "Illegal instr"
    newPos = (\(a0, b0) (a1, b1) -> (a0 + a1, b0 + b1)) m pos
  in
    if (decodeNumber pad newPos) == 'X' then pos else newPos

findNumber :: [String] -> (Int, Int) -> String -> (Int, Int)
findNumber pad start = foldr (\c pos -> move pad c pos) start 

solve :: [String] -> (Int, Int) -> [String] -> [(Int, Int)]
solve _ _ [] = []
solve pad start (l:lns) =
  let
    curr = (findNumber pad start l)
  in curr:(solve pad curr lns)

padP1 :: [String]
padP1 = ["XXXXX",
         "X123X",
         "X456X",
         "X789X",
         "XXXXX"]
startP1 = (2, 2)

padP2 :: [String]
padP2 = ["XXXXXXX",
         "XXX1XXX",
         "XX234XX",
         "X56789X",
         "XXABCXX",
         "XXXDXXX",
         "XXXXXXX"]
startP2 = (1, 3)

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ case args of
                      (v:_) -> v
                      _ -> "input.txt"
  let
    lns = map reverse $ lines input
    partOne = map (decodeNumber padP1) $ solve padP1 startP1 lns
    partTwo = map (decodeNumber padP2) $ solve padP2 startP2 lns

  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
