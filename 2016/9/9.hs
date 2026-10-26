import System.IO
import System.Environment
import Data.Maybe (fromMaybe, listToMaybe)


parseMarker :: String -> Maybe (Int, Int, String)
parseMarker str =
  let
    (nStr, rest) = break (== 'x') str
  in case rest of
    ('x': rest') ->
      let (mStr, rest'') = break (== ')') rest'
      in case rest'' of
        (')': rem) -> Just (read nStr, read mStr, rem)
        _ -> Nothing
    _ -> Nothing

parse :: String -> String
parse [] = []
parse [s] = [s]
parse (s:str)
  | s == '(' = case parseMarker str of
                    Just (n, m, s) ->
                      let
                        pre = concat $ replicate m $ take n s
                        post = parse $ drop n s
                      in pre ++ post
                    Nothing -> (s:parse str)
  | otherwise = (s:parse str)


parseP2 :: String -> Int
parseP2 [] = 0
parseP2 [s] = 1
parseP2 (s:str)
  | s == '(' = case parseMarker str of
                    Just (n, m, s) ->
                      let
                        pre = take n s
                        preLn = parseP2 pre
                        post = drop n s
                      in (m * preLn) + parseP2 post
                    Nothing -> 1 + parseP2 str
  | otherwise = 1 + parseP2 str

testsP1 :: [(String, Int)]
testsP1 =
   [("ADVENT", 6),
    ("A(1x5)BC", 7),
    ("(3x3)XYZ", 9), -- XYZXYZXYZ
    ("A(2x2)BCD(2x2)EFG", 11), -- ABCBCDEFEFG
    ("(6x1)(1x3)A", 6),
    ("X(8x2)(3x3)ABCY", 18)] --X(3x3)ABC(3x3)ABCY

testsP2 :: [(String, Int)]
testsP2 =
   [("(3x3)XYZ", 9), -- XYZXYZXYZ
    ("X(8x2)(3x3)ABCY", 20),-- XABCABCABCABCABCABCY
    ("(27x12)(20x12)(13x14)(7x10)(1x12)A", 241920),
    ("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN", 445)]

runTests :: (String -> Int) -> [(String, Int)] -> [String]
runTests fun cases = map (\(s, n) ->
                           let
                             ln = fun s
                           in
                             s ++ " " ++ (show n) ++ " " ++ 
                             if ln == n
                             then "== " ++ (show ln) ++ " : OK"
                             else "/= " ++ (show ln) ++ " : NOK") cases

main :: IO ()
main = do
  input <- readFile . fromMaybe "input.txt" . listToMaybe =<< getArgs
  let
    compressed = init input

    partOne = length $ parse compressed
    partTwo = parseP2 compressed

--  putStrLn "Part One Tests:"
--  mapM_ putStrLn $ runTests (length . parse) testsP1
--  putStrLn "Part Two Tests:"
--  mapM_ putStrLn $ runTests parseP2 testsP2

  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
