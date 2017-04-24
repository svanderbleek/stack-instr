{-

- string
- integer

| Instruction      | Description                                  | Stack size |
|------------------+----------------------------------------------+------------|
| ~PUSH <literal>~ | Add =literal= to the top of the stack.       |         +1 |
| ~POP~            | Discard the top item of the stack.           |         -1 |
| ~ADD~            | Add the top two items of the stack together. |         -1 |
| ~PRINT~          | Print the top item of the stack.             |         -1 |

int + int = int
str + str = str
int + str = str
str + int = str


  PUSH ", really"
  PUSH " years old"
  PUSH 5
  PUSH "I am "
  ADD
  ADD
  ADD
  PRINT
 
---
 
  PUSH "A"
  ([], ["A"])
  PRINT
  (["A"],[])
  PUSH "B"
  (["A"],["B"])
  PRINT
  (["B","A"],[])
-}

data Data = DI Int | DS String

data Instruction
  = Push Data
  | Pop
  | Add
  | Print
  
type Effects = ([Data], [Data])  

runStack :: [Instruction] -> Effects
runStack instructions = 
  iterate intructions ([],[]) 
  
iterate :: [Instructions] -> Effects -> Effects
iterate [] state = state
iterate (instr:instructions) state =
  iterate instructions (runStack' state instr)
    
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
