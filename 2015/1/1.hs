import System.IO

pos :: Char -> Int
pos '(' = 1
pos ')' = -1
pos _ = 0

findBasement :: Int -> Int -> String -> Int
findBasement i p [] = -1
findBasement i p (s:str)
  | (pos s) + p == -1 = i
  | otherwise = findBasement (i + 1) ((pos s) + p) str

main :: IO ()
main = readFile "input.txt" >>= \dirs -> do
  putStrLn $ ("Part One: " ++) $ show $ foldr ((+) . pos) 0 dirs
  putStrLn $ ("Part Two: " ++) $ show $ findBasement 1 0 dirs 
{-
main = do
    content <- readFile "input.txt"
    let position =  foldr ((+) . pos) 0 content
    putStrLn $ ("Part One: " ++) $ show position
    let basement = findBasement 1 0 content
    putStrLn $ ("Part Two: " ++) $ show basement
-}
