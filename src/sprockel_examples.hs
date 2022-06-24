module SprockelExamples (main2) where

import Sprockell

-- let x = 10;
-- x = 1;
-- print(x);

prog1 :: [Instruction]
prog1 =
  [ Load (ImmValue 2) regA,
    WriteInstr regA numberIO,
    EndProg
  ]

prog2 :: [Instruction]
prog2 =
  [ Load (ImmValue 5) regA,
    WriteInstr regA numberIO,
    EndProg
  ]

main2 :: IO ()
main2 = runWithDebugger (debuggerSimplePrint showLocalMem) [prog1, prog2]

showLocalMem :: DbgInput -> String
showLocalMem (_, systemState) = show $ localMem $ head $ sprStates systemState

-- let v = 7;
-- {
  -- let a = 10;
  -- let b = 20;
  -- let c = 30;
  -- let d = 40;
  -- let x = a + b;
  -- let y = c + d;
  -- let z = c + d;
  -- print(x);
  -- print(y);
-- }
-- print(v);





testProg :: [Instruction]
testProg =
  [
    Load (ImmValue 7) regA,
    Push regA
  ]
  ++
  [
    Load (ImmValue (-10)) regA,    -- a -> 31
    Push regA,
    Load (ImmValue 20) regA,    -- b -> 30
    Push regA,
    Load (ImmValue 30) regA,    -- c -> 29
    Push regA,
    Load (ImmValue 40) regA,    -- d -> 28
    Push regA,

    Load (ptr (0+1)) regA,
    Load (ptr (1+1)) regB,
    Compute Add regA regB regA, -- regA = a + b
    Push regA,                   -- x = a + b -> 27

    Load (ptr (2+1)) regA,
    Load (ptr (3+1)) regB,
    Compute Add regA regB regA, -- regA = c + d
    Push regA,                   -- y = c + d -> 26

    Load (ptr (2+1)) regA,
    Load (ptr (3+1)) regB,
    Compute Add regA regB regA, -- regA = c + d
    Push regA,                  -- z = c + d -> 26=5

    Load (ptr (4+1)) regA,      -- print x
    WriteInstr regA numberIO,

    Load (ptr (5+1)) regA,      -- print y
    WriteInstr regA numberIO,

    Load (ptr (6+1)) regA,      -- print z
    WriteInstr regA numberIO,

    Pop regA,
    Pop regA,
    Pop regA,
    Pop regA,
    Pop regA,
    Pop regA,
    Pop regA
  ]
  ++
  [
    Load (ptr (0+0)) regA,      -- print v
    WriteInstr regA numberIO,

    Pop regA,

    EndProg
  ]

manyVarProg :: [Instruction]
manyVarProg =
  [ -- push d
    Load (ImmValue 40) regA,
    Push regA,
    -- push c
    Load (ImmValue 30) regA,
    Push regA,
    -- push b
    Load (ImmValue 20) regA,
    Push regA,
    -- push a
    Load (ImmValue 10) regA,
    Push regA,
    -- x = a + b
    Pop regA,
    Pop regB,
    Compute Add regA regB regC,
    -- y = c + d
    Pop regA,
    Pop regB,
    Compute Add regA regB regA,
    -- push y
    Push regA,
    -- push x
    Push regC,
    -- print(x)
    Pop regA,
    WriteInstr regA numberIO,
    -- print(y)
    Pop regA,
    WriteInstr regA numberIO,
    EndProg
  ]

runProgDebug :: [Instruction] -> IO ()
runProgDebug prog = runWithDebugger (debuggerSimplePrint showLocalMem) [prog]
