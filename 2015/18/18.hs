import System.IO
import System.Environment
import Graphics.Gloss

data Zipper a = Zipper [a] a [a] deriving (Show, Functor, Foldable, Traversable)

incr :: Zipper a -> Maybe (Zipper a)
incr (Zipper _ _ []) = Nothing
incr (Zipper ps c (n:ns)) = Just $ Zipper (c:ps) n ns

decr :: Zipper a -> Maybe (Zipper a)
decr (Zipper [] _ _) = Nothing
decr (Zipper (p:ps) c ns) = Just $ Zipper ps p (c:ns)

curr :: Zipper a -> a
curr (Zipper _ c _) = c

fromList :: [a] -> Zipper a
fromList (x:xs) = Zipper [] x xs

toList :: Zipper a -> [a]
toList (Zipper ps c ns) = reverse ps ++ [c] ++ ns

type Grid a = Zipper (Zipper a)

fromListG :: [[a]] -> Grid a
fromListG = fromList . map fromList

toListG :: Grid a -> [[a]]
toListG = (map toList) . toList

parseLines :: [String] -> Grid Bool
parseLines = fromListG . map (map (== '#'))

up, down, right, left :: Grid a -> Maybe (Grid a)
up    = decr
down  = incr
right = traverse incr
left  = traverse decr

currG :: Grid a -> a
currG = curr . curr

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
  (=>>) :: ( w a -> b) -> w a -> w b
  (=>>) = extend

instance Comonad Zipper where
  extract = curr
  duplicate zpr = Zipper (go decr zpr) zpr (go incr zpr)
    where
      go f v = case f v of
        Nothing -> []
        Just v' -> v':go f v'

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g = \a -> f a >>= \b -> g b

duplicateG :: Grid a -> Grid (Grid a)
duplicateG g = fmap duplicateHor (duplicate g)
  where 
    duplicateHor g = Zipper (go left g) g (go right g)
      where
        go f v = case f v of
          Nothing -> []
          Just v' -> v' : go f v'

neighborFs :: [Grid a -> Maybe (Grid a)]
neighborFs =
  [up, up >=> right, right, right >=> down,
   down, down >=> left, left, left >=> up]

getNeighbors :: Grid a -> [a]
getNeighbors grd = concatMap go neighborFs
  where
    go f = case f grd of
      Nothing -> []
      Just v -> [currG v]

cellRules :: Int -> Bool -> Bool
cellRules 2 True = True
cellRules 3 _ = True
cellRules _ _ = False

stepCell :: Grid Bool -> Bool
stepCell grd = 
  let
   c = currG grd
   nNbrs = length $ filter id $ getNeighbors grd
  in cellRules nNbrs c


stepG :: Grid Bool -> Grid Bool
stepG g = fmap (fmap stepCell) (duplicateG g)

isCorner :: Grid a -> Bool
isCorner grd = length (getNeighbors grd) == 3

stepCell2 :: Grid Bool -> Bool
stepCell2 grd
  | isCorner grd = True
  | otherwise = 
  let
   c = currG grd
   nNbrs = length $ filter id $ getNeighbors grd
  in cellRules nNbrs c

stepG2 :: Grid Bool -> Grid Bool
stepG2 g = fmap (fmap stepCell2) (duplicateG g)

cellSize, gridSize, gridOffset :: Float
cellSize = 5.0
gridSize = 100.0 * cellSize
gridOffset = gridSize / 2

drawGrid :: Grid Bool -> Picture
drawGrid grid = Pictures
  [ translate (x * cellSize - gridOffset) (gridOffset - y * cellSize) 
      $ color (light green) 
      $ rectangleSolid (cellSize - 1) (cellSize - 1)
  | (y, row)  <- zip [0..] (toListG grid)
  , (x, cell) <- zip [0..] row , cell]

main :: IO ()
main = do
  args <- getArgs
  input <- readFile (if null args then "input.txt" else head args)
  let
    grid = parseLines $ lines input
    grid2 = fmap (fmap (\c -> if isCorner c then True else currG c)) (duplicateG grid)
    p1Solve = iterate stepG grid !! 100
    p2Solve = iterate stepG2 grid2 !! 100
    partOne = length $ filter id $ concat $ map toList $ toList p1Solve
    partTwo = length $ filter id $ concat $ map toList $ toList p2Solve


  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
  
--  simulate
--    (InWindow "GOL" (900, 900) (100, 100))
--    black 10 grid2 drawGrid (\vp t g -> stepG2 g)
