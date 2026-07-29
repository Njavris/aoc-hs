import System.IO
import System.Environment
import Data.List

incrPwd :: String -> String
incrPwd ('z':str) = 'a':(incrPwd str)
incrPwd (c:str) = (succ c):str

infPwds :: String -> [String]
infPwds pwd =
  let
    next = incrPwd pwd
  in next : (infPwds next)

confusingLetters = "iol" :: String

hasNoConfusingLetters :: String -> Bool
hasNoConfusingLetters = (0 ==) . length . filter (flip elem confusingLetters)

hasIncrLetters :: String -> Bool
hasIncrLetters (_:_:[]) = False
hasIncrLetters (_:[]) = False
hasIncrLetters [] = False
hasIncrLetters (a:b:c:rest)
  | succ c == b && succ b == a = True
  | succ c == b = hasIncrLetters (b:c:rest)
  | otherwise = hasIncrLetters (c:rest)

findPairs :: String -> [String]
findPairs [] = []
findPairs (_:[]) = []
findPairs (a:b:str)
 | a == b = [a,b]:(findPairs str)
 | otherwise = findPairs (b:str)

has2Pairs :: String -> Bool
has2Pairs = (2 <= ) . length . findPairs

partOneRules :: String -> Bool
partOneRules str =
  hasNoConfusingLetters str &&
  hasIncrLetters str &&
  has2Pairs str

main :: IO ()
main = head <$> getArgs >>= \input -> do
  let
    pwds = infPwds $ reverse input
    next = take 2 $ filter partOneRules pwds
    partOne = reverse $ next !! 0
    partTwo = reverse $ next !! 1

  putStrLn $ "Part One: " ++  partOne
  putStrLn $ "Part Two: " ++  partTwo
