import System.IO
import System.Environment
import Data.Word

data Reg = A | B deriving (Show, Eq)

data ISA = HLF Reg |
           TPL Reg |
           INC Reg |
           JMP Int |
           JIE Reg Int |
           JIO Reg Int deriving (Show, Eq)

data CPU = CPU
  { a :: !Word
  , b :: !Word
  , pc :: !Int
  } deriving (Show)

parseReg :: String -> Reg
parseReg ('a':_) = A
parseReg ('b':_) = B
parseReg v = error $ ("Invalid register: " ++ v)

parseOff :: String -> Int
parseOff ('+':rest) = read rest :: Int
parseOff str = read str :: Int

parseLine :: String -> ISA
parseLine str =
  case words str of
  ("hlf":reg:[]) -> HLF (parseReg reg)
  ("tpl":reg:[]) -> TPL (parseReg reg)
  ("inc":reg:[]) -> INC (parseReg reg)
  ("jmp":off:[]) -> JMP (parseOff off)
  ("jie":reg:off:[]) -> JIE (parseReg reg) (parseOff off)
  ("jio":reg:off:[]) -> JIO (parseReg reg) (parseOff off)
  _ -> error $ ("Unhandled instr: " ++ str)

parseInput :: String -> [ISA]
parseInput input = go $ lines input
  where
    go [] = []
    go (ln:rest) = (parseLine ln) : (go rest)


modPc :: Int -> CPU -> CPU
modPc off cpu = cpu { pc = pc cpu + off }

pcInc :: CPU -> CPU
pcInc = modPc 1

getReg :: Reg -> CPU -> Word
getReg A = a
getREg B = b

modReg :: Reg -> (Word -> Word) -> CPU -> CPU
modReg A f cpu = cpu { a = f (a cpu) }
modReg B f cpu = cpu { b = f (b cpu) }

-- processInstr :: ISA -> CPU -> CPU
-- processInstr (HLF r) cpu = (pcInc . modReg r (`div` 2)) cpu
-- processInstr (TPL r) cpu = (pcInc . modReg r (* 3)) cpu
-- processInstr (INC r) cpu = (pcInc . modReg r (+ 1)) cpu
-- processInstr (JMP off) cpu = modPc off cpu
-- processInstr (JIE r off) cpu = if even (getReg r cpu) then modPc off cpu else pcInc cpu
-- processInstr (JIO r off) cpu = if getReg r cpu == 1 then modPc off cpu else pcInc cpu

-- Point free
cond :: (CPU -> Bool) -> (CPU -> CPU) -> (CPU -> CPU) -> CPU -> CPU
cond pred f g cpu = if pred cpu then f cpu else g cpu

processInstr :: ISA -> CPU -> CPU
processInstr (HLF r) = pcInc . modReg r (`div` 2)
processInstr (TPL r) = pcInc . modReg r (* 3)
processInstr (INC r) = pcInc . modReg r (+ 1)
processInstr (JMP off) = modPc off
processInstr (JIE r off) = cond (even . getReg r) (modPc off) pcInc
processInstr (JIO r off) = cond ((== 1) . getReg r) (modPc off) pcInc

stepProgram :: [ISA] -> CPU -> CPU
stepProgram prog cpu =
  let
    curr = pc cpu
    instr = prog !! curr
    cpu' = processInstr instr cpu
    curr' = pc cpu'
  in
    if curr' >= (length prog) || curr' < 0
    then cpu'
    else stepProgram prog cpu'


main :: IO ()
main  = do
  args <- getArgs
  let fn = case args of
           (v:_) -> v
           _ -> "input.txt"
  input <- readFile fn
  let
    prog = parseInput input
    part = (show . b . stepProgram prog)
    partOne = part CPU { a = 0, b = 0, pc = 0 }
    partTwo = part CPU { a = 1, b = 0, pc = 0 }

  putStrLn $ ("Part One: " ++) partOne
  putStrLn $ ("Part Two: " ++) partTwo
