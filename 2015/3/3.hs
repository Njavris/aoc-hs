import System.IO
import qualified Data.Set as Set

move :: Char -> (Int, Int)
move 'v' = (0 , 1)
move '^' = (0 , -1)
move '>' = (1 , 0)
move '<' = (-1 , 0)

position :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
position (px, py) ((mx, my):mtl) = (px, py) : (position (px + mx, py + my) mtl)
position pos []  = [pos]

main :: IO ()
main = readFile "input.txt" >>= \input -> do
  let
    moves = map move input
    numUniquePos = Set.size . Set.fromList . position (0, 0)
    partOne = numUniquePos moves

    idxedMoves = zip [0 ..] moves
    oddMoves = position (0,0) [m | (i, m) <- idxedMoves, odd i]
    evenMoves = position (0,0) [m | (i, m) <- idxedMoves, even i]
    partTwo = Set.size $ Set.fromList $ oddMoves ++ evenMoves

    

  putStrLn $ ("Part One: " ++) $ show $ partOne
  putStrLn $ ("Part Two: " ++) $ show $ partTwo
