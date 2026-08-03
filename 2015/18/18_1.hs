import System.IO
import System.Environment
import Data.Array
import Graphics.Gloss

toList :: String -> [[Bool]]
toList = map (map (== '#')) . lines

fromList :: [[Bool]] -> String
fromList = unlines . map (map (\b -> if b then '#' else '.'))

listToArray :: [[Bool]] -> Array (Int, Int) Bool
listToArray lst =
  let
    h = length lst - 1
    w = length (lst !! 0) - 1
  in array ((0, 0), (w, h)) [((x, y), (lst !! y) !! x) | x <- [0 .. w], y <- [0 .. h]]

arrayToList :: Array (Int, Int) Bool -> [[Bool]]
arrayToList arr =
  let
    ((_, _), (w, h)) = bounds arr
  in [[arr ! (x, y) | x <- [0 .. w]] | y <- [0 .. h]]

toArray :: String -> Array (Int, Int) Bool
toArray = listToArray . toList

fromArray :: Array (Int, Int) Bool -> String
fromArray = fromList . arrayToList

neighborDirs :: [(Int, Int)]
neighborDirs =
  [(-1, -1), (-1, 0), (-1, 1),
   (0, -1), (0, 1), (1, -1),
   (1, 0), (1, 1)]

getNeighbors :: Array (Int, Int) Bool -> (Int, Int) -> Int
getNeighbors arr pos = sum [go pos (x, y) | (x, y) <- neighborDirs]
  where
    bds = bounds arr
    go (x0, y0) (x, y)
      | inRange bds  nbr = if arr ! nbr then 1 else 0
      | otherwise = 0
      where nbr = (x0 + x, y0 + y)

cellRules :: Int -> Bool -> Bool
cellRules 2 True = True
cellRules 3 _ = True
cellRules _ _ = False

step :: Array (Int, Int) Bool -> Array (Int, Int) Bool
step arr = array bds [(pos, updateCell pos) | pos <- range bds]
  where
    bds = bounds arr
    updateCell pos = cellRules (getNeighbors arr pos) (arr ! pos)

updateCorners :: Array (Int, Int) Bool -> Array (Int, Int) Bool
updateCorners arr = arr // [((0, 0), True),((mX, mY), True), ((mX, 0) , True), ((0, mY) , True)]
    where ((_, _), (mX, mY)) = bounds arr

stepP2 :: Array (Int, Int) Bool -> Array (Int, Int) Bool
stepP2 arr = array bds [(pos, updateCell pos) | pos <- range bds]
  where
    bds@((_, _), (mX, mY)) = bounds arr
    isCorner pos
      | pos == (0, 0) || pos == (mX, mY) || pos == (mX, 0) || pos == (0, mY) = True
      | otherwise = False
    updateCell pos = isCorner pos || (cellRules (getNeighbors arr pos) (arr ! pos))

data SimState = SimState
  { grid  :: Array (Int, Int) Bool
  , stepN :: Int
  }

initialState :: Array (Int, Int) Bool -> SimState
initialState g = SimState { grid = g, stepN = 0 }

stepSim :: Array (Int, Int) Bool -> SimState -> SimState
stepSim _ (SimState g n) = SimState (stepP2 g) (n + 1)

drawSim :: SimState -> Picture
drawSim (SimState g n) = Pictures [drawCells, drawHUD]
  where
    ((_, _), (w, h)) = bounds g
    cellSize = 8.0
    offsetX  = fromIntegral w * cellSize / 2
    offsetY  = fromIntegral h * cellSize / 2

    drawCells = Pictures
      [ translate (fromIntegral x * cellSize - offsetX)
                  (offsetY - fromIntegral y * cellSize)
          $ color (light green)
          $ rectangleSolid (cellSize - 1) (cellSize - 1)
      | ((x, y), True) <- assocs g]

    liveCount = length . filter id $ elems g
    hudText = "Step: " ++ show n ++ " | Live: " ++ show liveCount
    drawHUD = translate (-380) 380
            $ scale 0.15 0.15
            $ color white
            $ Text hudText

main :: IO ()
main = do
  args <- getArgs
  input <- readFile (if null args then "input.txt" else head $ args)
  let
    grid = toList input
    h = length grid
    w = length $ head grid
    arr = listToArray grid
    partOneSolve = iterate step arr !! 100
    partOne = length $ filter id $ elems partOneSolve

    arrP2 = updateCorners arr
    partTwoSolve = iterate stepP2 arrP2 !! 100
    partTwo = length $ filter id $ elems partTwoSolve

  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part One: " ++) $ show partTwo
  simulate (InWindow "GOL" (900, 900) (100, 100))
    black 10 (initialState arrP2) drawSim (\_ _ state -> stepSim arrP2 state)
