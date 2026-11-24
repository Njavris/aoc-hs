import System.IO
import System.Environment
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Char (isDigit)

type Getter = Cpu -> Int
type Setter = Cpu -> Int -> Cpu

data Arg = Reg Getter Setter | Val Int

data Instr =
  Cpy (Arg) (Arg) |
  Jnz (Arg) (Arg) |
  Inc (Arg) |
  Dec (Arg)

data Cpu = Cpu
  { regPc :: !Int
  , regA :: !Int
  , regB :: !Int
  , regC :: !Int
  , regD :: !Int
  } deriving (Show)

setA, setB, setC, setD :: Setter
setA cpu v = cpu { regA = v }
setB cpu v = cpu { regB = v }
setC cpu v = cpu { regC = v }
setD cpu v = cpu { regD = v }

valGet :: Cpu -> Arg -> Int
valGet cpu (Reg get _ ) = get cpu
valGet _ (Val v) = v

processInstr :: Cpu -> Instr -> Cpu
processInstr cpu (Cpy a (Reg _ set)) = set cpu $ valGet cpu a
processInstr cpu (Jnz a1 a2)
 | valGet cpu a1 /= 0 = cpu { regPc = regPc cpu + valGet cpu a2 - 1 }
 | otherwise = cpu
processInstr cpu (Inc (Reg get set)) = set cpu $ get cpu + 1
processInstr cpu (Dec (Reg get set)) = set cpu $ get cpu - 1
processInstr _ _ = error "Unexpected: "

run :: Cpu -> [Instr] -> Cpu
run cpu prog
  | regPc cpu  < 0 || regPc cpu >= length prog = cpu
  | otherwise =
    let
      instr = prog !! regPc cpu
      cpu' = processInstr cpu instr
      cpu'' = cpu' { regPc = regPc cpu' + 1 }
    in run cpu'' prog

isNum :: String -> Bool
isNum ('-':str) = not (null str) && all isDigit str
isNum str = not (null str) && all isDigit str

parseArg :: String -> Arg
parseArg arg
 | isNum arg = Val (read arg)
parseArg "a" = Reg regA setA
parseArg "b" = Reg regB setB
parseArg "c" = Reg regC setC
parseArg "d" = Reg regD setD
parseArg arg = error $ "unexpected" ++ arg

parseLine :: String -> Instr
parseLine ln = go $ words ln
  where
    go :: [String] -> Instr
    go ["cpy",arg1,arg2] = Cpy (parseArg arg1) (parseArg arg2)
    go ["jnz",arg1,arg2] = Jnz (parseArg arg1) (parseArg arg2)
    go ["inc",arg] = Inc (parseArg arg)
    go ["dec",arg] = Dec (parseArg arg)
    go v = error $ ("unexpected " ++) $ show v

main :: IO ()
main = do
  input <- getArgs >>= readFile . fromMaybe "input.txt" . listToMaybe
  let
    prog = map parseLine $ lines input
    initCpu = Cpu { regPc = 0, regA = 0, regB = 0, regC = 0, regD = 0 }
    partOne = regA $ run initCpu prog
    initCpu' = initCpu { regC = 1 }
    partTwo = regA $ run initCpu' prog

  putStrLn $ ("Part One: " ++) $ show partOne
  putStrLn $ ("Part Two: " ++) $ show partTwo
