import System.IO
import System.Environment
import Data.List

type RainDeer = (String, Int, Int, Int)

parseLine :: String -> RainDeer
parseLine ln =
  let
    wds = words ln
    nm = wds !! 0
    velocity = read (wds !! 3) :: Int
    run = read (wds !! 6) :: Int
    rest = read (wds !! 13) :: Int
  in (nm, velocity, run, rest)

targetTime = 2503 :: Int

calcDist :: Int -> RainDeer -> Int
calcDist duration (nm, velocity, runT, restT) =
  let
    periodT = runT + restT
    periodX = runT * velocity
    nPeriods = div duration periodT
    wholeX = nPeriods * periodX
    remainderT = mod duration periodT
    remainderX = velocity * min runT remainderT
  in wholeX + remainderX

partOneSolve :: [RainDeer] -> [Int]
partOneSolve = map (calcDist targetTime)

raindeerVelStream :: RainDeer -> [Int]
raindeerVelStream (nm, velocity, runT, restT) = 
  cycle $ map (\t -> if t <= runT then velocity else 0) [1 .. (runT + restT)]

raindeerPosStreams :: [RainDeer] -> [[Int]]
raindeerPosStreams = transpose . map (scanl1 (+) . raindeerVelStream)

scoreRaindeers :: Int -> [[Int]] -> [[Int]]
scoreRaindeers dur = take dur . map (\tmp ->
    let mx = maximum tmp
    in map (\v -> if v == mx then 1 else 0) tmp)

partTwoSolve :: [RainDeer] -> [Int]
partTwoSolve = map sum . transpose . scoreRaindeers targetTime . raindeerPosStreams

main :: IO ()
main = head <$> getArgs >>= readFile >>= \input -> do
  let
    lns = lines input
    stats = map parseLine lns
    partOne = maximum $ partOneSolve stats
    partTwo = maximum $ partTwoSolve stats 
  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
