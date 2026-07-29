import System.Environment (getArgs)
import System.IO

length' :: String -> Int
length' [] = -2
length' ('\\':'\\':rest) = 1 + length' rest
length' ('\\':'\"':rest) = 1 + length' rest
length' ('\\':'x':_:_:rest) = 1 + length' rest
length' (_:rest) = 1 + length' rest

length'' :: String -> Int
length'' [] = 2
length'' ('\"':rest) = 2 + length'' rest
length'' ('\\':rest) = 2 + length'' rest
length'' (_:rest) = 1 + length'' rest

main :: IO ()
main = getArgs >>= readFile . head >>= \input -> do
  let
    lns = lines input
    totalChars = sum $ map length lns
    totalChars' = sum $ map length' lns
    totalChars'' = sum $ map length'' lns
    partOne = totalChars - totalChars'
    partTwo = totalChars'' - totalChars
  putStrLn $ ("Part One: " ++ ) $ show partOne
  putStrLn $ ("Part Two: " ++ ) $ show partTwo
