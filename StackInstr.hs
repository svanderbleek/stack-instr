module StackInstr where

data Data
  = DI Int
  | DS String
  deriving (Show, Eq)

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

-- | Stack evaluation
--
-- Examples:
--
-- >>> runStack [pushs ", really", pushs " years old", pushi 5, pushs "I am ", Add, Add, Add, Print]
-- I am 5 years old, really

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
runStack' (out, d1:d2:rest) Add =
  (out, (d1 <> d2):rest)
runStack' stack Add =
  stack
runStack' (out,(head:rest)) Print =
  (head:out, rest)
runStack' state@(_,[]) Print = state

-- | Data Combinations
--
-- Examples:
--
-- prop> \x y z -> (DI x <> DI y) <> DI z == DI x <> (DI y <> DI z)
-- prop> \x y z -> (DS x <> DS y) <> DS z == DS x <> (DS y <> DS z)
-- prop> \x y -> isDI (DI x <> DI y)
-- prop> \x y -> isDS (DS x <> DS y)
-- prop> \x y -> isDS (DS x <> DI y)
-- prop> \x y -> isDS (DI x <> DS y)

(<>) :: Data -> Data -> Data
(DI int1) <> (DI int2) = DI (int1 + int2)
(DS str1) <> (DS str2) = DS (str1 ++ str2)
(DI int1) <> (DS str2) = DS ((show int1) ++ str2)
(DS str1) <> (DI int2) = DS (str1 ++ (show int2))

isDS :: Data -> Bool
isDS (DI _) = False
isDS (DS _) = True

isDI :: Data -> Bool
isDI (DS _) = False
isDI (DI _) = True
