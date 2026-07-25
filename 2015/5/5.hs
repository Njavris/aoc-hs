import System.IO
import Data.List

-- Part one
containsInvalidPairs :: String -> Bool
containsInvalidPairs str = any (`isInfixOf` str) ["ab", "cd", "pq", "xy"]

containsRepeatingPairs :: String -> Bool
containsRepeatingPairs (a:b:rest) = a == b || containsRepeatingPairs (b:rest)
containsRepeatingPairs _ = False

wovels = "aeiou" :: String

containsNWovels :: Int -> String -> Bool
containsNWovels n str = (n <=) $ length $ filter (\c -> elem c wovels) str

partOneRules :: String -> Bool
partOneRules str =
  (not $ containsInvalidPairs str) &&
    (containsRepeatingPairs str) &&
    (containsNWovels 3 str)

-- Part Two
containsRepeatingPair :: String -> Bool
containsRepeatingPair (a:b:str) = (isInfixOf (a:b:[]) str) || containsRepeatingPair (b:str)
containsRepeatingPair _ = False

containsSplitDouble :: String -> Bool
containsSplitDouble (a:b:c:rest) = a == c || containsSplitDouble (b:c:rest)
containsSplitDouble _ = False

partTwoRules :: String -> Bool
partTwoRules str = containsSplitDouble str && containsRepeatingPair str

main :: IO ()
main = readFile "input.txt" >>= \input -> do
  let lns = lines input
  putStrLn $ ("Part One: " ++) $ show $ length $ filter partOneRules lns
  putStrLn $ ("Part Two: " ++) $ show $ length $ filter partTwoRules lns
