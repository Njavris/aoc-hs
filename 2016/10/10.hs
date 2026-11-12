import System.IO
import System.Environment
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)


data NodeId = BotT Int | OutT Int deriving (Show, Eq)
data NodeVal = BotV [Int] | OutV Int deriving (Show)
data Node = Node
  { node :: NodeId
  , act :: Maybe (NodeId, NodeId)
  , val :: Maybe NodeVal
  } deriving (Show)

parseLine :: String -> (Maybe Node, Maybe (NodeId, Int))
parseLine ln = go $ words ln
  where
    mkNode t i = case t of
      "bot" -> BotT (read i :: Int)
      "output" -> OutT (read i :: Int)
    go ["bot",id,_,"low",_,t1,n1,_,"high",_,t2,n2] =
        (Just Node { node = BotT (read id :: Int),
              act = Just (mkNode t1 n1, mkNode t2 n2), val = Nothing }, Nothing)
    go ["value",v,_,_,t,n] =
      (Nothing, Just (mkNode t n, read v :: Int))
    go v = error $ ("Unforseen input: " ++) $ show v

parseInput :: [String] -> ([Node], [(NodeId, Int)])
parseInput ins =
  let
    (graphMyb, valsMyb) = unzip $ map parseLine ins
  in
    (catMaybes graphMyb, catMaybes valsMyb)


assign :: [Node] -> (NodeId, Int) -> [Node]
assign bots (bot, v) = updateBot bots
  where
    updateBot :: [Node] -> [Node]
    updateBot [] =
      case bot of
        OutT id -> [Node {node = bot, act = Nothing, val = Just (OutV v)}]
        _ -> []
    updateBot (b:bs) =
      if node b == bot
        then
          let
            val' = case val b of
              Just (BotV v1) -> BotV (v:v1)
              Nothing -> BotV [v]
          in (Node { node = node b, act = act b, val = Just val' }:bs)
      else (b:updateBot bs)

clearBot :: [Node] -> NodeId -> [Node]
clearBot [] _ = []
clearBot (b:bs) i
  | node b == i = b { val = Just (BotV []) } : bs
  | otherwise = b : clearBot bs i

nextReady :: [Node] -> Maybe (Node, [Int])
nextReady [] = Nothing
nextReady (b:bs) =
  let
    val' = case val b of
      Just (BotV v) -> Just v
      _ -> Nothing
  in case val' of
    Just v | length v >= 2 -> Just (b, v)
    _ -> nextReady bs

botPresent :: NodeId -> [Node] -> Bool
botPresent _ [] = False
botPresent nd (n:ns) = if node n == nd then True else botPresent nd ns

solve :: (Int, Int) -> [Node] -> (Maybe Int, [Node])
solve (mnT, mxT) bots =
  case nextReady bots of
    Nothing -> (Nothing, bots)
    Just (b, v) ->
      let
        (mnB, mxB) = case act b of
          Just (b1, b2) -> (b1, b2)
          _ -> error $ "Unexpected"
        mn = minimum v
        mx = maximum v
        isP1 = mn == mnT && mx == mxT
        g' = assign bots (mnB, mn)
        g'' = assign g' (mxB, mx)
        g''' = clearBot g'' $ node b
        isP2 = botPresent (OutT 0) g''' &&
               botPresent (OutT 1) g''' &&
               botPresent (OutT 2) g'''
        i = case node b of
          BotT i -> Just i
          _ -> Nothing
      in if isP1 && isP2 then (i, g''') else solve (mnT, mxT) g'''


main :: IO ()
main = do
  input <- readFile . fromMaybe "input.txt" . listToMaybe =<< getArgs
  let
    instrs = lines input
    (parsed, vals) = parseInput instrs
    initialGraph = foldr (\v g -> assign g v) parsed vals

    (partOne, graph) = solve (17, 61) initialGraph
    graph' = filter (\b -> case node b of
                             OutT i -> i == 0 || i == 1 || i == 2
                             _ -> False ) graph
    partTwo = product $ map (\o -> case val o of
                           Just (OutV v) -> v
                           _ -> error "Unexpected") graph'

  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
