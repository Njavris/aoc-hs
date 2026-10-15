import System.IO
import System.Environment

data Dir = R Int | L Int deriving (Show)

parseInput :: String -> [Dir]
parseInput input =
  let
    go :: String -> Dir
    go ('R':rest) = R (read rest :: Int)
    go ('L':rest) = L (read rest :: Int)

    wrds = words $ filter (\a -> a /= ',') input
  in
    map go wrds


start = (1, 0)

steps :: Dir -> Int
steps (R n) = n
steps (L n) = n

turn :: Dir -> (Int, Int) -> (Int, Int)
turn d p =
  let
    rotateL = (\(x, y) -> (y * 1, x * (-1)))
    rotateR = (\(x, y) -> (y * (-1), x * 1))
  in case d of
    (R _) -> rotateR p
    (L _) -> rotateL p

route :: (Int, Int) -> [Dir] -> [(Int, Int)]
route _ [] = []
route orient (d:dir) =
  let
    walk = (\(x, y) dir -> let n = steps dir in (x * n, y * n))
    newOrient = turn d orient
  in
    (walk newOrient d):route newOrient dir

manhDist :: (Int, Int) -> Int
manhDist (x, y) = abs x + abs y

solveP1 :: [(Int, Int)] -> (Int, Int)
solveP1 = foldr (\(x, y) (x1, y1) -> (x1 + x, y1 + y)) (0, 0)

step = (\(x0, y0) (x, y) -> (x0 + x, y0 + y))

routeP2 :: (Int, Int) -> [(Int, Int)] -> Int -> [Dir] -> (Int, Int)
routeP2 orient (r:route) rem dirs
  | rem > 0 =
    let
      newNode = step r orient
      isDupe = elem newNode (r:route)
    in if isDupe then newNode else routeP2 orient (newNode:r:route) (rem - 1) dirs
routeP2 orient route 0 (d:dirs) = routeP2 (turn d orient) route (steps d) dirs
routeP2 _ _ 0 [] = error "Failed to find"

main :: IO ()
main = do
  args <- getArgs
  let fn = case args of
           (v:_) -> v
           _ -> "input.txt"
  input <- readFile fn

  let
    dirs = parseInput input
    partOne = manhDist $ solveP1 $ route start dirs
    partTwo = manhDist $ routeP2 start [(0, 0)] 0 dirs

  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
