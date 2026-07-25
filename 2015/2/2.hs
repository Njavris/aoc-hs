import System.IO

box :: String -> (Int, Int, Int)
box ln = case map read  $ splitOnX ln of
  [a, b, c] -> (a, b, c)
  _ -> error "Failed"

splitOnX :: String -> [String]
splitOnX str = case break (== 'x') str of
  (part, "") -> [part]
  (part, _:more) -> part : splitOnX more

calcArea :: (Int, Int, Int) -> Int
calcArea (a, b, c) =
  let
    ab = a * b
    bc = b * c
    ac = a * c
    extra = min ab $ min bc ac
  in 2 * (ab + bc + ac) + extra
  
calcRibbon :: (Int, Int, Int) -> Int
calcRibbon (a, b, c) =
  let
    ab = 2 * (a + b)
    bc = 2 * (b + c)
    ac = 2 * (a + c)
    perim = min ab $ min bc ac
  in perim + (a * b * c)

main :: IO ()
main = readFile "input.txt" >>= \input -> do
  let
    lns = map box $ lines input
    totalArea = foldr ((+) . calcArea) 0 lns
    totalRibbon = foldr ((+) . calcRibbon) 0 lns

  putStrLn $ ("Part One: " ++) $ show totalArea
  putStrLn $ ("Part Two: " ++) $ show totalRibbon
