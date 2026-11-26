import System.IO
import System.Environment
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (tails)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.ByteString (ByteString)

rule1 :: String -> Maybe Char
rule1 (a1:a2:a3:rest)
  | a1 == a2 && a2 == a3 = Just a1
  | otherwise = rule1 (a2:a3:rest)
rule1 _ = Nothing

rule2 :: Char -> Int -> Int -> Int -> IntMap String -> Bool
rule2 ch cnt curr tgt m
  | curr >= tgt = False
  | otherwise =
    if rpts hash 5 ch
    then True
    else rule2 ch cnt curr' tgt m
    where
      hash = case IntMap.lookup curr m of
        Just str -> str
        _ -> error "Unexpected"
      cnt' = if elem ch hash then cnt - 1 else cnt
      curr' = curr + 1

      rpts :: String -> Int -> Char -> Bool
      rpts h n ch
       | n <= 0 = False
       | otherwise = any same windows
         where
           same as = all (== ch) as
           windows = filter ((== n) . length) $ map (take n) $ tails h

calcMD5 :: (String -> String) -> String -> Int -> Int -> IntMap String -> IntMap String
calcMD5 _ _ _ 0 m = m
calcMD5 hashFun seed curr cnt m = calcMD5 hashFun seed curr' cnt' m'
    where
      curr' = curr + 1
      cnt' = cnt - 1
      hash =  hashFun $ seed <> show curr
      m' = case IntMap.lookup curr m of
        Just h ->
          if hash == h
          then m
          else error "Unexpected"
        _ -> IntMap.insert curr hash m

solve :: (String -> String) -> String -> Int -> Int -> IntMap String -> [(Int, String)]
solve _ _ _ 0 _ = []
solve hashFun seed curr tgt cache =
   let
     curr' = curr + 1
     upper = curr' + 1000
     cache' = calcMD5 hashFun seed upper 1 cache
     hash = case IntMap.lookup curr cache of
       Just str -> str
       _ -> error "Unexpected"
     rules = case rule1 hash of
       Just c -> rule2 c 5 curr' upper cache
       _ -> False
   in
     if rules
     then (curr, hash):solve hashFun seed curr' (tgt - 1) cache'
     else solve hashFun seed curr' tgt cache'

toStr :: ByteString -> String
toStr = concatMap byteToHex . B.unpack
  where
    hexDigits = "0123456789abcdef"
    byteToHex w =
      [ hexDigits !! fromIntegral (w `div` 16)
      , hexDigits !! fromIntegral (w `mod` 16)
      ]

p1Hash :: String -> String
p1Hash = toStr . MD5.hash . C8.pack

p2Hash :: String -> String
p2Hash = go 2017
  where
    go 0 s = s
    go cnt s = go (cnt - 1) $ p1Hash s

main :: IO ()
main = do
  seed <- getArgs >>= return . fromMaybe "abc" . listToMaybe
  let
    cache = \f -> calcMD5 f seed 0 1001 (IntMap.empty :: IntMap String)
    part = \f -> fst $ last $ solve f seed 0 64 (cache f)
  putStrLn $ ("Part One: " ++) $ show $ part p1Hash
  putStrLn $ ("Part Two: " ++) $ show $ part p2Hash
