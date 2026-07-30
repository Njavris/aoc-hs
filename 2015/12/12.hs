import System.IO
import System.Environment
import Data.Char

data JNode = JObj [(String, JNode)] |
  JArr [JNode] | JStr String | JNum Int deriving Show

data Tkn = TOpenBrc | TCloseBrc | TOpenBrkt | TCloseBrkt | TColon |
  TComma | TString String | TNumber Int deriving (Show, Eq)

tokenize :: String -> [Tkn]
tokenize [] = []
tokenize ('{':str) = TOpenBrc:tokenize str
tokenize ('}':str) = TCloseBrc:tokenize str
tokenize ('[':str) = TOpenBrkt:tokenize str
tokenize (']':str) = TCloseBrkt:tokenize str
tokenize (':':str) = TColon:tokenize str
tokenize (',':str) = TComma:tokenize str
tokenize ('\"':str) =
  let
    (s, rest) = break (== '"') str
  in (TString s):tokenize (drop 1 rest)
tokenize (a:str)
 | isDigit a =
   let
     (n, rest) = span isDigit (a:str)
   in (TNumber (read n :: Int)):tokenize rest
 | (a == '-' && isDigit (head str)) =
   let
     (n, rest) = span isDigit str
   in (TNumber (read (a:n) :: Int)):tokenize rest
 | otherwise = tokenize str

parseVal :: [Tkn] -> (JNode, [Tkn])
parseVal ((TNumber num):rest) = (JNum num, rest)
parseVal ((TString str):rest) = (JStr str, rest)
parseVal (TOpenBrkt:rest) =
  let (arr, rest1) = parseArr rest
  in ((JArr arr), rest1)
parseVal (TOpenBrc:rest) =
  let (obj, rest1) = parseObj rest
  in ((JObj obj), rest1)
parseVal (t:rest) = error ("Invalid " ++ (show t))

parseKeyVal :: [Tkn] -> ((String, JNode), [Tkn])
parseKeyVal ((TString str):TColon:rest) =
  let (val, rest1) = parseVal rest
  in ((str, val), rest1)

parseObj :: [Tkn] -> ([(String, JNode)], [Tkn])
parseObj (TCloseBrc:rest) = ([], rest)
parseObj tkns@((TString _):_) =
  let
    (kv, rest1) = parseKeyVal tkns
    rest2 = case rest1 of
      (TComma:rest3) -> rest3
      rest3 -> rest3
    (kvs, rest) = parseObj rest2
  in (kv:kvs, rest)
parseObj e = error ("Invalid " ++ (show e))

parseArr :: [Tkn] -> ([JNode], [Tkn])
parseArr (TCloseBrkt:rest) = ([], rest)
parseArr tkns =
  let
    (val, rest1) = parseVal tkns
    rest2 = case rest1 of
      (TComma:rest3) -> rest3
      rest3 -> rest3
    (vals, rest) = parseArr rest2
  in (val:vals, rest)

partOneSolve :: [Tkn] -> Int
partOneSolve [] = 0
partOneSolve ((TNumber n):rest) = n + partOneSolve rest
partOneSolve (_:rest) = partOneSolve rest

partTwoSolve :: JNode -> Int
partTwoSolve (JStr _) = 0
partTwoSolve (JNum n) = n
partTwoSolve (JArr arr) = sum $ map partTwoSolve arr
partTwoSolve (JObj kvs)
  | any (\(_, v) ->
    case v of
      JStr "red" -> True
      _ -> False) kvs = 0
  | otherwise = sum $ map (\(_, v) -> partTwoSolve v) kvs

main :: IO ()
main = head <$> getArgs >>= readFile >>= \input -> do
  let
    tokenized = tokenize input
    (obj, []) = parseObj $ tail tokenized
    ast = JObj obj
    partOne = partOneSolve tokenized
    partTwo = partTwoSolve ast
  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
