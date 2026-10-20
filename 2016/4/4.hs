import System.IO
import System.Environment
import Data.Char (isDigit)
import Data.List (sort, group, sortOn, elemIndex, isInfixOf)

parseRoom :: String -> (String, Int, String)
parseRoom name =
  let
    undashed = filter (/= '-') name
    (prefix, rest) = break isDigit undashed
    (digits, rest1) = span isDigit rest
    suffix = filter (`notElem` "[]") rest1
  in
    (prefix, read digits :: Int, suffix)

validateRoom :: (String, Int, String) -> Bool
validateRoom (name, _, csum) = actualCsum == csum
  where
    grouped = group $ sort name
    flattened = zip (map head grouped) (map length grouped)
    sorted = sortOn (\(a, b) -> (negate b, a)) flattened
    actualCsum = map (\(a, b) -> a) $ take 5 sorted


decypherRoom :: (String, Int, String) -> (String, Int)
decypherRoom (cypher, n, _) = (map go cypher, n)
  where
    alphabet = "abcdefghijklmnopqrstuvwxyz"
    m = n `mod` length alphabet
    infAlphabet = cycle alphabet
    go c = case elemIndex c infAlphabet of
             Just v -> infAlphabet !! (v + m)
             _ -> error "Error!"

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ case args of
                        (fn:_) -> fn
                        _ -> "input.txt"
  let
    rooms = lines input
    parsedRooms = map parseRoom rooms
    validRooms = filter (validateRoom) parsedRooms
    partOne = foldr (\(_, id, _) s -> s + id) 0 validRooms
    decypheredRooms = map decypherRoom validRooms
    partTwo = filter (\(nm, _) -> "northpoleobjects" `isInfixOf` nm) decypheredRooms

  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
