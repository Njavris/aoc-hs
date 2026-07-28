import System.IO
import System.Environment
import Data.List
import qualified Data.Map as Map

data Dist = Dist String String Int deriving (Show, Eq)

parseLine :: [String] -> Dist
parseLine [x1, "to", x2, "=", dst] = Dist x1 x2 (read dst :: Int)

uniqueCities :: [Dist] -> [String]
uniqueCities = nub . concatMap (\(Dist x1 x2 _) -> [x1, x2])

type DistMap = Map.Map (String, String) Int

roads :: [Dist] -> DistMap
roads = Map.fromList . concatMap (\(Dist x1 x2 v) -> [((x1, x2), v), ((x2, x1), v)])

calcDistance :: DistMap -> [String] -> Int
calcDistance rds route = sum $ map (rds Map.!) $ zip route (tail route)

main :: IO ()
main = getArgs >>= readFile . head >>= \input -> do
  let
    dists = map (parseLine . words) $ lines input
    routes = permutations $ uniqueCities dists
    roadsMap = roads dists
    distances = map (calcDistance roadsMap) routes

  putStrLn $ ("Part One: " ++) $ show $ minimum distances
  putStrLn $ ("Part Two: " ++) $ show $ maximum distances
