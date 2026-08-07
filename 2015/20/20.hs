import System.IO
import System.Environment
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import Control.Monad (forM_)

sieve :: Int -> Int -> Maybe Int -> V.Vector Int
sieve n inc mx = V.create $ do
  vector <- MV.replicate (div n inc) 0
  forM_ [1 .. n] $ \d -> do
    let
      limit = (div n inc)
      stopAt = case mx of
        Nothing -> limit
	Just v -> min limit (v * d)
      nextMultiple m
        | m >= stopAt = pure ()
	| otherwise = MV.unsafeModify vector (+ (d * inc)) m >> nextMultiple (m + d)
    nextMultiple (d - 1)
  pure vector

solvePartOne :: Int -> Int
solvePartOne tgt =
  case V.findIndex (>= tgt) (sieve tgt 10 Nothing) of 
    Just x -> x + 1
    Nothing -> error "increase vector size for part one"

solvePartTwo :: Int -> Int
solvePartTwo tgt =
  case V.findIndex (>= tgt) (sieve tgt 11 (Just 50)) of
    Just x -> x + 1
    Nothing -> error "increase vector size for part two"

main :: IO ()
main = do
  args <- getArgs
  let
    input = if null args then "130" else head args
    target = read input :: Int
    partOne = solvePartOne target
    partTwo = solvePartTwo target
  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
