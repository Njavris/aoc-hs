import System.IO
import System.Environment
import Data.Char (isDigit)
import qualified Data.Set as Set

data Game = Game
  { bossHp :: !Int -- Boss HP
  , bossDmg :: !Int -- Boss DMG
  , playerHp :: !Int -- Player HP
  , playerMn :: !Int -- Player Mana
  , shieldT :: !Int -- Shield effect remaining Turns
  , poisonT :: !Int -- Poison effect rmainint Turns
  , rechargeT :: !Int -- Recharge effect remaining Turns
  , totalMnUsed :: !Int -- Total used mana - result
  , playerTurn :: !Bool -- Is it players turn
  , finished :: !Bool -- Is game finished
  } deriving (Show, Eq, Ord)

parseBoss :: String -> Game
parseBoss input =
  let
    lns = lines input
    intfy idx = (read :: String -> Int) . filter isDigit $ lns !! idx
  in
    Game
     { bossHp = intfy 0
     , bossDmg = intfy 1
     , playerHp = 50
     , playerMn = 500
     , shieldT = 0
     , poisonT = 0
     , rechargeT = 0
     , totalMnUsed = 0
     , playerTurn = True
     , finished = False}

data Spell = MagicMissile | Drain | Shield | Poison | Recharge deriving (Show)

spellCost :: Spell -> Int
spellCost MagicMissile = 53
spellCost Drain = 73
spellCost Shield = 113
spellCost Poison = 173
spellCost Recharge = 229

castSpell :: Game -> Spell -> Maybe Game
castSpell game spl =
  let
    cost = spellCost spl
    pMn = playerMn game
    pMn1 = pMn - cost
    totalMn = totalMnUsed game + cost
    game' =  game { totalMnUsed = totalMn, playerMn = pMn1, playerTurn = False}
  in case compare pMn1 0 of
    LT -> Nothing
    _ -> applySpell game' spl

applyEffects :: Game -> Bool -> Game
applyEffects game p2 =
  let
    bHp = bossHp game
    poison = poisonT game
    bHp1 = if (poison > 0) then max (bHp - 3) 0 else bHp
    pHp = if p2 then playerHp game - 1 else playerHp game
    pMn = playerMn game
    recharge = rechargeT game
    pMn1 = if (recharge > 0) then pMn + 101 else pMn
    shield = shieldT game
  in game
    { bossHp = bHp1
    , playerHp = pHp
    , playerMn = pMn1
    , shieldT = max (shield - 1) 0
    , poisonT = max (poison - 1) 0
    , rechargeT = max (recharge - 1) 0 }

applySpell :: Game -> Spell -> Maybe Game
applySpell game MagicMissile = Just game { bossHp = bossHp game - 4 }
applySpell game Drain = Just game { bossHp = bossHp game - 2, playerHp = playerHp game + 2 }
applySpell game Shield =
  if shieldT game > 0
  then Nothing
  else Just game { shieldT = 6 }
applySpell game Poison =
  if poisonT game > 0
  then Nothing
  else Just game { poisonT = 6 }
applySpell game Recharge =
  if rechargeT game > 0
  then Nothing
  else Just game { rechargeT = 5 }

bossTurn :: Game -> Game
bossTurn game =
  let
    shT = shieldT game
    bDmg = bossDmg game
    pHp = playerHp game
    pHp1 = pHp - (if shT > 0 then max (bDmg - 7) 1 else bDmg)
  in game { playerHp = pHp1, playerTurn = True}

turn :: Game -> Spell -> Bool -> Maybe Game
turn game spl p2 =
  let appliedEff = applyEffects game p2
  in
    if playerHp appliedEff <= 0
    then Nothing
    else
      if bossHp appliedEff <= 0
      then Just appliedEff { finished = True }
      else case castSpell appliedEff spl of
        Nothing -> Nothing
        Just game' ->
          if bossHp game' <= 0
          then Just game' { finished = True }
          else
            if playerHp game' <= 0
            then Nothing
            else
              let appliedEff' = applyEffects game' False
              in if bossHp appliedEff' <= 0
                 then Just appliedEff' { finished = True }
                 else
                   let game'' = bossTurn appliedEff'
                   in if playerHp game'' <= 0
                      then Nothing
                      else Just game''

spellBook :: [Spell]
spellBook = [MagicMissile, Drain, Shield, Poison, Recharge]

branchGame :: Game -> Bool -> [Game]
branchGame game p2 = [ game' | spl <- spellBook, Just game' <- [turn game spl p2]]

solveDijkstra :: Game -> Bool -> Maybe Int
solveDijkstra game p2 = go (Set.singleton (totalMnUsed game, game)) Set.empty
  where
    go prQueue visited
      | Set.null prQueue = Nothing
      | finished game' = Just mnUsed
      | Set.member game' visited = go prQueue' visited
      | otherwise = go nextPrQueue nextVisited
      where
        ((mnUsed, game'), prQueue') = Set.deleteFindMin prQueue
        nextVisited = Set.insert game' visited
        nextStates = [ (totalMnUsed g, g) | g <- (branchGame game' p2), not (Set.member g visited)]
        nextPrQueue = foldr Set.insert prQueue' nextStates


main :: IO ()
main = do
  args <- getArgs
  let fn = case args of
            (v:_) -> v
            _ -> "input.txt"
  input <- readFile fn
  let
    initialGame = parseBoss input
    part p2 = case solveDijkstra initialGame p2 of
      Just mn -> show mn
      _ -> show "failed to find solution"
    partOne = part False
    partTwo = part True

  putStrLn $ ("Part One: " ++) partOne
  putStrLn $ ("Part Two: " ++) partTwo
