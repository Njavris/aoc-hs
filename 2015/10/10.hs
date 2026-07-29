import System.IO
import System.Environment

processNumberOnce :: String -> String
processNumberOnce "" = ""
processNumberOnce (s:str) =
  let
    (curr, next) = span (== s) str
    n = 1 + length curr
  in (show n) ++ [s] ++ (processNumberOnce next)

processNumberNTimes :: Int -> String -> String
processNumberNTimes n
  | n > 0 = processNumberNTimes (n - 1) . processNumberOnce
  | otherwise = id


main :: IO ()
main = head <$> getArgs >>= \input -> do
  let
    partOne = length $ processNumberNTimes 40 input
    partTwo = length $ processNumberNTimes 50 input
  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
