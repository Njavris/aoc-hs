import System.IO
import System.Environment
import Data.List

parseLine :: String -> [Int]
parseLine ln =
  let
    wds = words ln
    capacity = read (init (wds !! 2)) :: Int
    durability = read (init (wds !! 4)) :: Int
    flavor = read (init (wds !! 6)) :: Int
    texture = read (init (wds !! 8)) :: Int
    calories = read (wds !! 10) :: Int
  in capacity:durability:flavor:texture:calories:[]

calcScore :: [[Int]] -> [Int] -> Int
calcScore ingr ord = product $ map (max 0) $ init $ map sum $ transpose $ zipWith (\v -> map (* v)) ord ingr

calcScore500cal :: [[Int]] -> [Int] -> Int
calcScore500cal ingr ord =
  let
    totals = map sum $ transpose $ zipWith (\v -> map (* v)) ord ingr
    calories = last totals
    propertyScores = map (max 0) (init totals)
  in if calories == 500 then product propertyScores else 0

main :: IO ()
main = head <$> getArgs >>= readFile >>= \input -> do
  let
    parsed = map parseLine $ lines input
    orders = [x:y:z:(100 - x - y - z):[] | x <- [0 .. 100], y <- [0 .. 100 - x], z <- [0 .. 100 - x - y]]

    partOne = maximum $ map (calcScore parsed) orders
    partTwo = maximum $ map (calcScore500cal parsed) orders
  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
