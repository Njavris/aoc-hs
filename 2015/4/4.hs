import System.IO
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS
import Text.Printf (printf)
import Data.List (find)

input = ""

isSucc1 :: String -> Bool
isSucc1 ('0':'0':'0':'0':'0':_) = True
isSucc1 _ = False

isSucc2 :: String -> Bool
isSucc2 ('0':'0':'0':'0':'0':'0':_) = True
isSucc2 _ = False

md5sum :: String -> String
md5sum = concatMap (printf "%02x") . BS.unpack . MD5.hash . BS.pack

testNumber :: (String -> Bool) -> Int -> Bool
testNumber test = test . md5sum . (input ++ ) . show

partOne :: Maybe Int
partOne = find (testNumber isSucc1) [1..]

partTwo :: Maybe Int
partTwo = find (testNumber isSucc2) [1..]

main :: IO ()
main = do
  putStrLn $ ("Part One: " ++ ) $ case partOne of Just r -> show r
  putStrLn $ ("Part Two: " ++ ) $ case partTwo of Just r -> show r

