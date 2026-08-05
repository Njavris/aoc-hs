import System.IO
import System.Environment
import Data.List
import Data.Tuple (swap)

parseRule :: String -> (String, String)
parseRule ln = case words ln of
  [a, "=>", b] -> (a, b)
  _ -> error $ "Failed to parse rule: " ++ ln

parseMorphs :: [String] -> [(String, String)]
parseMorphs lns = map parseRule lns

morphString :: [(String, String)] -> String -> [String]
morphString morphs str =
  let
    strLen = length str - 1
  in [pre ++ new ++ drop (length patt) post
    | (patt, new) <- morphs,
    i <- [0 .. strLen],
    let (pre, post) = splitAt i str, 
    isPrefixOf patt post]

morphSingleString :: (String, String) -> String -> Maybe String
morphSingleString (patt, new) str = go "" str
  where
    pLen = length patt
    go _ [] = Nothing
    go pre post@(p:ps)
      | isPrefixOf patt post = Just $ reverse pre ++ new ++ drop pLen post
      | otherwise = go (p:pre) ps
    
morphStringGreedy :: [(String, String)] -> String -> String
morphStringGreedy [] str = error $ "No pattern matched string: " ++ str
morphStringGreedy (m:morphs) str =
  case morphSingleString m str of
    Nothing -> morphStringGreedy morphs str
    Just s -> s

partTwoSolve :: [(String, String)] -> String -> Int
partTwoSolve morphs str
  | str == "e" = 0
  | otherwise = 1 + (partTwoSolve morphs $ morphStringGreedy morphs str)


main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ if null args then "input.txt" else head args

  let
    lns = filter (not . null) $ lines input
    morphs = parseMorphs $ init lns
    formula = last lns
    partOne = length $ nub $ morphString morphs formula
    invMorphs =
      sortBy (\(a, _) (b, _) -> compare (length b) (length a)) $
      map swap morphs
    partTwo = partTwoSolve invMorphs formula
  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
