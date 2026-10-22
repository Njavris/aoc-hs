import System.IO
import System.Environment
import Data.List (isInfixOf)
import Debug.Trace (trace)

hasAbba :: String -> Bool
hasAbba [] = False
hasAbba (a1:b1:b2:a2:rest)
  | a1 == a2 && b1 == b2 && a1 /= b1 = True
  | otherwise = hasAbba (b1:b2:a2:rest)
hasAbba _ = False

data Networks = Supernet String | Hypernet String deriving (Show)
type IP = [Networks]

parseIp :: String -> IP
parseIp [] = []
parseIp str = case break (\c -> c == '[' || c == ']') str of
  (seg, '[' : rest) -> Supernet seg : parseIp rest
  (seg, ']' : rest) -> Hypernet seg : parseIp rest
  ([], []) -> []
  (seg, []) -> [Supernet seg]

hasHypernetF :: (String -> Bool) -> IP -> Bool
hasHypernetF _ [] = False
hasHypernetF f ((Supernet _):ips) = hasHypernetF f ips
hasHypernetF f ((Hypernet str):ips)
  | f str = True
  | otherwise = hasHypernetF f ips


hasSupernetF :: (String -> Bool) -> IP -> Bool
hasSupernetF f [] = False
hasSupernetF f ((Hypernet _):ips) = hasSupernetF f ips
hasSupernetF f ((Supernet str):ips)
  | f str = True
  | otherwise = hasSupernetF f ips

hasBab :: IP -> String -> Bool
hasBab ((Supernet str):rest) bab = hasBab rest bab
hasBab ((Hypernet str):rest) bab
--  | trace ("hasBAB: " ++ bab) False = undefined
  | isInfixOf bab str = True
  | otherwise = hasBab rest bab
hasBab _ _ = False

hasAba :: IP -> String -> Bool
hasAba ip (a1:b:a2:rest)
--  | trace ("hasABA: " ++ [a1,b,a2]) False = undefined
  | a1 == a2 && a1 /= b && hasBab ip [b, a1, b] = True
  | otherwise = hasAba ip (b:a2:rest)
hasAba _ _ = False

main :: IO ()
main = do
  input <- getArgs >>= (\arg ->
                       readFile $ case arg of
                            (fn:_) -> fn
                            _ -> "input.txt")
  let
    ips = lines input
    parsed = map parseIp ips
    partOne = length $ filter (\ip -> (not $ hasHypernetF hasAbba ip) && (hasSupernetF hasAbba ip)) parsed
    partTwo = length $ filter (\ip -> (hasSupernetF (hasAba ip) ip)) parsed


  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
