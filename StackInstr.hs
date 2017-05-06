module StackInstr where

-- | Stack evaluation
--
-- Examples:
--
-- >>> runStack [pushs ", really", pushs " years old", pushi 5, pushs "I am ", Add, Add, Add, Print]
-- ([DS "I am 5 years old, really"],[])

data Data
  = DI Int
  | DS String
  deriving (Show)

data Instruction
  = Push Data
  | Pop
  | Add
  | Print
  deriving (Show)

pushs :: String -> Instruction
pushs = Push . DS

pushi :: Int -> Instruction
pushi = Push . DI

type Effects = ([Data], [Data])

runStack :: [Instruction] -> Effects
runStack instructions =
  interate' instructions ([],[])

interate' :: [Instruction] -> Effects -> Effects
interate' [] state = state
interate' (instr:instructions) state =
  interate' instructions (runStack' state instr)

runStack' :: Effects -> Instruction -> Effects
runStack' (out,stack) (Push d) =
  (out,d:stack)
runStack' state@(_,[]) Pop = state
runStack' (out,_:rest) Pop =
  (out,rest)
runStack' (out, data1:data2:rest) Add =
  (out, addData data1 data2 : rest)
runStack' stack Add =
  stack
runStack' (out,(head:rest)) Print =
  (head:out, rest)
runStack' state@(_,[]) Print = state

addData :: Data -> Data -> Data
addData (DI int1) (DI int2) = DI (int1 + int2)
addData (DS str1) (DS str2) = DS (str1 ++ str2)
addData (DI int1) (DS str2) = DS ((show int1) ++ str2)
addData (DS str1) (DI int2) = DS (str1 ++ (show int2))
