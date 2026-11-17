import System.IO
import System.Environment
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Sequence as Seq (Seq(..), singleton, (<|), (|>))
import Data.Set as Set (Set(..), notMember, insert, singleton)
import Data.List (tails, sort)

strToInt :: String -> Int
strToInt str
  | str == "first" = 1
  | str == "second" = 2
  | str == "third" = 3
  | str == "fourth" = 4

data StuffType = Generator String | Microchip String deriving (Show, Eq, Ord)
type Room = (Int, [StuffType])
type State = (Int, [Room])

isCompatible :: [StuffType] -> Bool
isCompatible sfs =
  let
    hasGenerators = any (\i -> case i of Generator _ -> True; _ -> False) sfs
    isItemSafe (Microchip n) = 
      not hasGenerators || elem (Generator n) sfs
    isItemSafe (Generator _) = True
  in
    all isItemSafe sfs

mutate :: State -> (Int, [StuffType]) -> Maybe State
mutate (fl, floors) (dl, items) =
  let
    nFl = fl + dl
    pIdx = fl - 1
    (rP, itmsP) = floors !! pIdx
    pFloor = (rP, sort $ filter (\a -> notElem a items) itmsP)
    nIdx = nFl - 1
    (rN, itmsN) = floors !! nIdx
    nFloor = (rN, sort $ itmsN ++ items)
    nFloors = (take pIdx floors) ++ [pFloor] ++ (drop (pIdx + 1) floors)
    nFloors' = (take nIdx nFloors) ++ [nFloor] ++ (drop (nIdx + 1) nFloors)
  in
    if nIdx < 0 || nIdx > 3 || not (isCompatible $ snd nFloor) || not (isCompatible $ snd pFloor)
      then Nothing
      else Just (nFl, nFloors')

isWin :: State -> Bool
isWin (fl, rooms) = all (\(r, itms) -> r == 4 || null itms) rooms

nextStates :: State -> [State]
nextStates state@(fl, floors) =
  let
    (_,items) = floors !! (fl - 1)
    singles = [[i] | i <- items]
    doubles = [[i, ii] | (i:is) <- tails items, ii <- is]
    iis = singles ++ doubles
    moves = [(dir, i) | i <- iis, dir <- [-1, 1]]
    mbNxts = map (mutate state) moves
  in [s | Just s <- mbNxts]


solve :: Seq (Int, State) -> Set State -> Maybe Int
solve Seq.Empty _ = Nothing
solve ((depth, state) :<| rest) cache
  | isWin state = Just depth
  | otherwise =
    let
      nxtSt = nextStates state
      unseen = filter (\s -> Set.notMember s cache) nxtSt
      cache' = foldr Set.insert cache unseen
      queue' = foldl (\q s -> q |> (depth + 1, s)) rest unseen
    in solve queue' cache'


parseLine :: [String] -> Room
parseLine ("The":fl:"floor":"contains":rest) =
  let
    parseStuff :: [String] -> [StuffType]
    parseStuff ("nothing":"relevant":_) = []
    parseStuff (tp:stuff:rest) =
      let
        tmp = case stuff of
          "generator" -> Generator tp
          "microchip" -> Microchip $ fst $ break (== '-') tp
          v -> error $ ("unexpected: " ++) $ show v
      in
        (tmp:parseStuff rest)
    parseStuff _ = []
    rest' = map (filter (`notElem` [',', '.'])) $ filter (`notElem` ["and","a"]) rest
    stuff = parseStuff rest'
  in
    (strToInt fl, stuff)
parseLine v = error $ ("unexpected: " ++) $ show v

main :: IO ()
main = do
  input <- readFile . fromMaybe "input.txt" . listToMaybe =<< getArgs
  let
    rooms = map (parseLine . words) $ lines input
    partOne = case solve (Seq.singleton (0, (1, rooms))) (Set.singleton (1, rooms)) of
      Just v -> v
      _ -> error "Something went wrong"

    p2 = [Generator "elerium", Microchip "elerium", Generator "dilithium", Microchip "dilithium"]
    rooms' = case rooms of
      (r, itms):rest -> (r, sort (itms ++ p2)):rest
      _ -> error "Something went wrong"
    partTwo = case solve (Seq.singleton (0, (1, rooms'))) (Set.singleton (1, rooms')) of
      Just v -> v
      _ -> error "Something went wrong"

  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
