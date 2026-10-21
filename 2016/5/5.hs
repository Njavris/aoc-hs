import System.IO
import System.Environment
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS
import Text.Printf (printf)
import Data.Char (ord)

md5 :: String -> String
md5 str = concatMap (printf "%02x") (BS.unpack . MD5.hash $ BS.pack str)


is5Zeros :: String -> Int -> Maybe String
is5Zeros str idx = case md5 tmp of
              ('0':'0':'0':'0':'0':char1:char2:rest) -> Just [char1, char2]
              _ -> Nothing
  where
    tmp = str ++ show idx

solve :: String -> Int -> [String]
solve str idx = case is5Zeros str idx of
                      Nothing -> solve str (idx + 1)
                      Just c -> c:[] ++ solve str (idx + 1)

replaceAtIdx :: Int -> Char -> String -> String
replaceAtIdx n c str = take n str ++ [c] ++ drop (n + 1) str


solveP2 :: String -> [String] -> String
solveP2 curr (l:list)
  | not $ elem 'X' curr = curr
  | otherwise = solveP2 newCurr list
    where
      idx = ord (head l) - ord '0'
      newCurr = if idx >= 0 && idx < 8 && (curr !! idx) == 'X'
                           then replaceAtIdx idx (last l) curr
                           else curr

main :: IO ()
main = do
  args <- getArgs
  let
    input = case args of
            (v:_) -> v
            _ -> error "No input"

  let
    solution = solve input 0
    partOne = map head $ take 8 solution
    partTwo = solveP2 "XXXXXXXX" solution

  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
