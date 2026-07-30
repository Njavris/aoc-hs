import System.IO
import System.Environment
import Data.List as List
import Data.Map as Map

parseLine :: String -> [((String, String), Int)]
parseLine ln = [((name1, name2), (sign * amnt))]
  where
    wds = words ln
    name1 = head wds
    name2 = init $ last wds
    amnt = read (wds !! 3) :: Int
    sign = if (wds !! 2) == "gain" then 1 else -1


type RuleMap = Map.Map (String,String) Int

getChange :: RuleMap -> (String, String)-> Int
getChange rm key = case Map.lookup key rm of
  Just v -> v
  Nothing -> 0

seatPeople :: [String] -> [(String, String)]
seatPeople ppl = zip ppl (tail (cycle ppl))

calcSeated :: RuleMap -> [String] -> Int
calcSeated rm seated = sum $ List.map (\(n1, n2) ->
  getChange rm (n1, n2) + getChange rm (n2,n1)) $ seatPeople seated

solve :: RuleMap -> [String] -> [Int]
solce _ [] = []
solve rm pers = List.map (calcSeated rm) $ permutations pers


main :: IO ()
main = head <$> getArgs >>= readFile >>= \input -> do
  let
    parsed = concatMap parseLine $ lines input
    rules = Map.fromList parsed
    persons = nub $ concatMap (\((nm1, nm2), _) -> nm1:nm2:[]) parsed
    partOne = maximum $ solve rules persons
    partTwo = maximum $ solve rules $ "me":persons
  putStrLn $ ("Part One: " ++ ) $ show partOne
  putStrLn $ ("Part Two: " ++ ) $ show partTwo
