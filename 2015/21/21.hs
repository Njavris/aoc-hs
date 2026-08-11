import System.IO
import System.Environment
import Data.Char (isDigit)
import Data.List (tails)

type Player = (String, Int, Int, Int)

-- input.txt format:
-- Hit Points: X
-- Damage: X
-- Armor: X
parseInput :: String -> String -> Player
parseInput nm str =
  let
    lnToInt = (read :: (String -> Int)) . filter isDigit
    hp = lnToInt $ lines str !! 0
    dmg = lnToInt $ lines str !! 1
    arm = lnToInt $ lines str !! 2
  in
    (nm, hp, dmg, arm)

-- p1 attacks p2
attack :: Player -> Player -> Player
attack p1@(nm1, hp1, dmg1, arm1) p2@(nm2, hp2, dmg2, arm2) =
  (nm2, hp2 - (max 1 (dmg1 - arm2)), dmg2, arm2)

fight :: Player -> Player -> Player
fight p1 p2 =
  let
    p2new@(nm2, hp2, dmp2, arm2) = attack p1 p2
  in
    if hp2 <= 0 then p1
    else fight p2new p1

type Item = (String, Int, Int, Int)

weapons :: [Item]
weapons =
  [("Dagger", 8, 4, 0),
   ("Shortsword", 10, 5, 0),
   ("Warhammer", 25, 6, 0),
   ("Longsword", 40, 7, 0),
   ("Greataxe", 74, 8, 0)]

armor :: [Item]
armor =
  [("None", 0, 0, 0),
   ("Leather", 13, 0, 1),
   ("Chainmail", 31, 0, 2),
   ("Splintmail", 53, 0, 3),
   ("Bandedmail", 75, 0, 4),
   ("Platemail", 102, 0, 5)]

rings :: [Item]
rings =
  [("None 1", 0, 0, 0),
   ("None 2", 0, 0, 0),
   ("Damage +1", 25, 1, 0),
   ("Damage +2", 50, 2, 0),
   ("Damage +3", 100, 3, 0),
   ("Defense +1", 20, 0, 1),
   ("Defense +2", 40, 0, 2),
   ("Defense +3", 80, 0, 3)]

permuteItems :: [[Item]]
permuteItems = [ (w:a:r1:r2:[]) | w <- weapons, a <- armor, (r1:rs) <- tails rings, r2 <- rs]

calcPrice :: [Item] -> Int
calcPrice itms = sum $ map (\(_, p, _, _) -> p) itms

equipItems :: Player -> [Item] -> Player
equipItems (nm, hp, dmg, arm) itms =
  let
    (dmgMod, armMod) = foldr (\(_, _, d, a) (ds, as) -> (ds + d, as + a)) (0, 0) itms
  in (nm, hp, dmg + dmgMod, arm + armMod)

isWinningBuild :: Player -> Player -> [Item] -> Bool
isWinningBuild boss pl@(nm, _, _, _) itms =
  let
    (wNm, _, _, _) = fight (equipItems pl itms) boss
  in nm == wNm

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "input.txt" else head args
  input <- readFile filename

  let
    boss = parseInput "Boss" input
    player = ("Player", 100, 0, 0)
    winningItms =
      filter (\itms -> (isWinningBuild boss player itms) == True) permuteItems
    winningItmsPrc = map calcPrice winningItms
    partOne = minimum winningItmsPrc
    losingItms =
      filter (\itms -> (isWinningBuild boss player itms) == False) permuteItems
    losingItmsPrc = map calcPrice losingItms
    partTwo = maximum losingItmsPrc

  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part One: " ++) $ show partTwo
