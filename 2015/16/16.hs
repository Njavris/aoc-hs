import System.IO
import System.Environment
import qualified Data.Map as Map

data AttrKey = Children | Cats | Samoyeds | Pomeranians | Akitas|
  Vizslas | Goldfish | Trees | Cars | Perfumes deriving (Show, Eq, Ord)

type Attr = (AttrKey, Int)


tickerTapeStr :: [(String, Int)]
tickerTapeStr =
    [("children", 3),
    ("cats", 7),
    ("samoyeds", 2),
    ("pomeranians", 3),
    ("akitas", 0),
    ("vizslas", 0),
    ("goldfish", 5),
    ("trees", 3),
    ("cars", 2),
    ("perfumes", 1)]

type AttrMap = Map.Map AttrKey Int

matchesAttr :: AttrMap -> Attr -> Bool
matchesAttr am (key, val) = case Map.lookup key am of
  Just v -> v == val
  Nothing -> error ("Error: " ++ show (key, val))

matchesPerson :: AttrMap -> (Int, [Attr]) -> Bool
matchesPerson am (_, attrs) = all (matchesAttr am) attrs

matchesAttrP2 :: AttrMap -> Attr -> Bool
matchesAttrP2 am (key, val) = case Map.lookup key am of
  Just v -> case key of
    Cats -> val > v
    Trees -> val > v
    Pomeranians -> val < v
    Goldfish -> val < v
    _ -> v == val
  Nothing -> error ("Error: " ++ show (key, val))

matchesPersonP2 :: AttrMap -> (Int, [Attr]) -> Bool
matchesPersonP2 am (_, attrs) = all (matchesAttrP2 am) attrs

parseAttribute :: (String, Int) -> Attr
parseAttribute ("children", v) = (Children, v)
parseAttribute ("cats", v) = (Cats, v)
parseAttribute ("samoyeds", v) = (Samoyeds, v)
parseAttribute ("pomeranians", v) = (Pomeranians, v)
parseAttribute ("akitas", v) = (Akitas, v)
parseAttribute ("vizslas", v) = (Vizslas, v)
parseAttribute ("goldfish", v) = (Goldfish, v)
parseAttribute ("trees", v) = (Trees, v)
parseAttribute ("cars", v) = (Cars, v)
parseAttribute ("perfumes", v) = (Perfumes, v)

parseLines :: String -> (Int, [Attr])
parseLines ln =
  let
    wds = words ln
    id = read (init $ wds !! 1) :: Int
    attr1 = init $ wds !! 2
    val1 = read (init $ wds !! 3) :: Int
    attr2 = init $ wds !! 4
    val2 = read (init $ wds !! 5) :: Int
    attr3 = init $ wds !! 6
    val3 = read (wds !! 7) :: Int

  in
    (id, [
      parseAttribute (attr1, val1),
      parseAttribute (attr2, val2),
      parseAttribute (attr3, val3)])

main :: IO ()
main = head <$> getArgs >>= readFile >>= \input -> do
  let
    lns = lines input
    tape = Map.fromList $ map parseAttribute tickerTapeStr 
    (partOne, attrs) = head $ filter (matchesPerson tape) $ map parseLines lns
    (partTwo, attrs2) = head $ filter (matchesPersonP2 tape) $ map parseLines lns
  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
