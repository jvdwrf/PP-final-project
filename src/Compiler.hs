module Compiler where

import qualified Data.Map as Map
import ParseTree
import Scope
import Sprockell
import Text.Printf (printf)
import TypeChecking

type SprilProg = [Instruction]

data SpawnCount = SC Int deriving (Show, Eq)

-- Compile an fml-program from a string into Sprockel code.
-- This will throw errors for any compilation errors.
--
-- The basic idea here is that we compile a bunch of different processes.
-- Only the first one will initialize all global variables, after which it will
-- wake the other processes.
--
-- Every process will therefore have a custom predetermined start- and end-program
-- That makes it wait until the moment all variables are initialized, and the process
-- is spawned by another process.
compile :: String -> [SprilProg]
compile str = map (\(n, p) -> compileInitProg n rootScope sharedDecl ++ p ++ [EndProg]) numberedProcesses
  where
    -- First, we parse the entire program
    (ParseTree sharedDecl program) = takeRight (parseFML str)
    -- Then we create our root-scope from the shared declarations
    rootScope = sharedDecl2Scope sharedDecl
    -- Now we can compile all the root-statements into a bunch of processes
    (process, processes) = compileProcess rootScope (SC 0) program
    -- And number them, so that we know that only process 0 should initialize the shared variables
    numberedProcesses = (zip [0 ..] (process : processes))

-- Creates the initial synchronization around the shared data, based on the number of the process
compileInitProg :: Int -> Scope -> [Decl] -> SprilProg
compileInitProg i rootScope sharedDecl = compileSharedBlock (i == 0) (sharedVars rootScope) sharedDecl

-- Compile a single process, without synchronization around a shared-block, or an EndProg.
-- The first argument returned is this program, and the others are any programs that this process
-- has spawned.
compileProcess :: Scope -> SpawnCount -> [RootStat] -> (SprilProg, [SprilProg])
compileProcess _scope _sc [] = ([], [])
-- If the statement is a normal statement, then we can just compile the statement, and move on
-- with the new scope. This statement can not spawn any more programs internally.
compileProcess scope sc ((RootStatStat stat) : stats) = (prog ++ restProg, restProgs)
  where
    (newScope, prog) = compileStat scope stat
    (restProg, restProgs) = compileProcess newScope sc stats
-- If the statement is a spawn-statement, things are a little bit more complex.
-- We return the program inside the do-block + the rest of the program as the first return value.
-- The second return value are all programs that are spawned from this one. Processes can recursively
-- spawn more processes.
compileProcess scope (SC sc) ((SpawnStat spawnStats doStats) : stats) =
  (thisProgFull ++ restProg, spawnProgFull : doProgs ++ spawnProgs ++ restProgs)
  where
    -- First we compile the spawned process, given a fresh scope and a spawn-count + 1
    (spawnProg, spawnProgs) = compileProcess (newRootScope (sharedVars scope)) (SC (sc + 1)) spawnStats
    -- Now we add a prefix and postfix to this spawned program, that will wait until it is spawned,
    -- and notify the spawner when it has exited
    spawnProgFull = compileSpawnedProg (SC sc) ++ spawnProg ++ compileSpawnedExitProg (SC sc)
    -- We can now compile the current process, this will get the current scope, and a spawn-count equal to
    -- sc + 1 + amount_of_processes_spawned.
    (thisProg, doProgs) = compileProcess scope (SC (sc + 1 + length spawnProgs)) doStats
    -- Again, we add a prefix and post-fix
    thisProgFull = compileSpawnProg (SC sc) ++ thisProg ++ compileSpawnExitProg (SC sc)
    -- and then we continue compiling the code after the do-block
    (restProg, restProgs) = compileProcess scope (SC sc) stats

-- The program that runs before a process is spawned on the spawner-side
compileSpawnProg :: SpawnCount -> SprilProg
compileSpawnProg sc = compileActiveBarrier (scAddrSpawn sc)

-- The program that runs before a process is spawned on the spawned-side
compileSpawnedProg :: SpawnCount -> SprilProg
compileSpawnedProg sc = compilePassiveBarrier (scAddrSpawn sc)

-- The program that runs after a process is spawned on the spawned-side
compileSpawnedExitProg :: SpawnCount -> SprilProg
compileSpawnedExitProg sc = compileActiveBarrier (scAddrExit sc)

-- The program that runs after a process is spawned on the spawner-side
compileSpawnExitProg :: SpawnCount -> SprilProg
compileSpawnExitProg sc = compilePassiveBarrier (scAddrExit sc)

-- Synchronize on a given global variable in memory.
-- This will wait until the global memory is set, and only then continue
-- It is only valid for 2 processes, and will not work for more
compilePassiveBarrier :: Int -> SprilProg
compilePassiveBarrier addr =
  [ Load (ImmValue addr) regB,
    TestAndSet (IndAddr regB), -- Try and set addr to 1
    Receive regA,
    Branch regA (Rel 2), -- Check if this was successful
    Jump (Rel (10 + 4)), -- Not successful, which means that we are allowed to spawn now
    Load (ImmValue 0) regA,
    WriteInstr regA (IndAddr regB) -- Successful, so we reset it to 0
  ]
    ++ compileNops 10 -- wait
    ++ [Jump (Rel (-10 - 7))] -- and try again

-- Synchronize on a given global variable in memory.
-- This will set the global memory, and then continue.
-- It is only valid for 2 processes, and will not work for more
compileActiveBarrier :: Int -> SprilProg
compileActiveBarrier addr =
  [ Load (ImmValue addr) regB,
    TestAndSet (IndAddr regB), -- Try to set addr to 1
    Receive regA,
    Branch regA (Rel 4) -- Check if this was successful
  ]
    ++ compileNops 2 -- It was not successful, wait
    ++ [Jump (Rel (-2 - 4))] -- and try again

-- Insert x nop operations
compileNops :: Int -> SprilProg
compileNops i = [Nop | _ <- [1 .. i]]

-- Compiles the shared-block, given at the start of a program.
--
-- This will synchronize all processes, and make sure that only one process actually initializes the variables.
-- All other processes will wait on this process to be finished until starting their own programs.
compileSharedBlock :: Bool -> ScopeVars -> [Decl] -> SprilProg
compileSharedBlock isInit vars sharedDecls =
  if isInit
    then
      compileSharedBlockRec list
        ++ [ Load (ImmValue 0) regB,
             TestAndSet (IndAddr regB),
             Receive regA,
             Branch regA (Rel 2),
             Jump (Rel (-3))
           ]
    else
      [ Load (ImmValue 0) regB,
        ReadInstr (IndAddr regB),
        Receive regA,
        Branch regA (Rel 12)
      ]
        ++ compileNops 10
        ++ [ Jump (Rel (-10 - 3))
           ]
  where
    list = (zipWith (\(_, (a, b)) c -> (a, b, c)) (Map.toList vars) sharedDecls)

-- Recursive inner part of compileSharedBlock
compileSharedBlockRec :: [(Int, Type, Decl)] -> SprilProg
compileSharedBlockRec [] = []
compileSharedBlockRec ((i, _t, (_, expr)) : rest) =
  compileExpr (newRootScope Map.empty) expr
    ++ [ Pop regA,
         Load (ImmValue ((i + 1) * 2)) regB,
         WriteInstr regA (IndAddr regB)
       ]
    ++ compileSharedBlockRec rest

-- Compile a multiple statements.
--
-- This may modify the scope, since statements contain declarations.
compileStats :: Scope -> [Stat] -> (Scope, SprilProg)
compileStats scope [] = (scope, [])
compileStats scope (stat : stats) = (scope'', thisStatProg ++ restStatProg)
  where
    (scope', thisStatProg) = compileStat scope stat
    (scope'', restStatProg) = compileStats scope' stats

-- Compile a single statement.
--
-- The returned scope will be the new scope, after any assignment.
compileStat :: Scope -> Stat -> (Scope, SprilProg)
compileStat scope (DeclStat decl) = compileLocalDecl scope decl
compileStat scope (ExprStat expr) = (scope, compileExpr scope expr ++ [Pop regA])
compileStat scope (AssignStat ident expr) = (scope, compileAssignStat scope ident expr)
compileStat scope (IfStat condExpr ifStats elseStats) = (scope, compileIfStat scope condExpr ifStats elseStats)
compileStat scope (WhileStat condExpr stats) = (scope, compileWhileStat scope condExpr stats)
compileStat scope (BlockStat stats) = (scope, snd (compileStats scope stats))
compileStat scope (AcquireStat ident stats) = (scope, compileAcquireStat scope ident stats)

-- Compiles a single acquire statement.
--
-- This will busy-loop, until it can acquire the lock, then execute the statements, and reset the lock.
-- This can not modify the scope passed into it.
compileAcquireStat :: Scope -> Ident -> [Stat] -> SprilProg
compileAcquireStat scope ident stats = prog (lookupScopeLoc scope ident)
  where
    prog (LocalLoc _addr) = error ("Can't acquire local variable " ++ ident)
    prog (SharedLoc addr) =
      [ Load (sharedLockAddr addr) regB,
        TestAndSet (IndAddr regB),
        Receive regA,
        Branch regA (Rel (10 + 2))
      ]
        ++ compileNops 10
        ++ [ Jump (Rel (-10 - 3))
           ]
        ++ snd (compileStats scope stats)
        ++ [ Load (sharedLockAddr addr) regB,
             Load (ImmValue 0) regA,
             WriteInstr regA (IndAddr regB)
           ]

-- Compiles a single while statement
compileWhileStat :: Scope -> Expr -> [Stat] -> SprilProg
compileWhileStat scope condExpr stats =
  if typeOfCond == BoolType
    then compileWhileStatChecked scope condExpr stats
    else error ("While statement must have a condition of type BoolType but found " ++ show typeOfCond ++ ".")
  where
    typeOfCond = getExprType scope condExpr

-- Inner part of compileWhileStat, that has type-checking done for it
compileWhileStatChecked :: Scope -> Expr -> [Stat] -> SprilProg
compileWhileStatChecked scope condExpr stats =
  condProg -- now execute the condition
    ++ [ Pop regA, -- and store it into regA
         Branch regA (Rel 2), -- branch depending on regA
         Jump (Rel (length prog + 2)) -- go here if true, and escape the loop
       ]
    ++ prog -- otherwise, execute the prog again
    ++ [ Jump (Rel (- (length prog + 3 + length condProg)))
       ] -- and jump back to the top
  where
    (_, prog) = compileStats scope stats
    condProg = compileExpr scope condExpr

---- Pop all variables from the stack that are now out of scope
--popEndScope :: Scope -> SprilProg
--popEndScope scope = [(Pop regA) | _ <- [0 .. pushCount scope]]

-- Compile a single if-statement
compileIfStat :: Scope -> Expr -> [Stat] -> [Stat] -> SprilProg
compileIfStat scope condExpr ifStats elseStats =
  if typeOfCond == BoolType
    then compileIfStatChecked scope condExpr ifStats elseStats
    else error ("If statement must have a condition of type BoolType but found " ++ show typeOfCond ++ ".")
  where
    typeOfCond = getExprType scope condExpr

-- Inner part of compileIfStat, that has type-checking done for it
compileIfStatChecked :: Scope -> Expr -> [Stat] -> [Stat] -> SprilProg
compileIfStatChecked scope condExpr ifStats elseStats =
  condProg
    ++ [ Pop regA,
         Branch regA (Rel (length elseProg + 2))
       ]
    ++ elseProg
    ++ [Jump (Rel (length ifProg + 1))]
    ++ ifProg
  where
    (_, ifProg) = compileStats scope ifStats
    (_, elseProg) = compileStats scope elseStats
    condProg = compileExpr scope condExpr

-- Compile a single assignment
compileAssignStat :: Scope -> Ident -> Expr -> SprilProg
compileAssignStat scope ident expr =
  if exprType == varType
    then compileAssignStatChecked scope ident expr
    else error ("Cannot assign expression of type " ++ show exprType ++ " to variable " ++ ident ++ " of type " ++ show varType ++ ".")
  where
    exprType = getExprType scope expr
    varType = lookupScopeType scope ident

-- Inner part of compileAssignStat, that has type-checking done for it
compileAssignStatChecked :: Scope -> Ident -> Expr -> SprilProg
compileAssignStatChecked scope ident expr = exprProg ++ prog (lookupScopeLoc scope ident)
  where
    exprProg = compileExpr scope expr
    prog (LocalLoc addr) =
      [ Pop regA,
        Store regA (ptr addr)
      ]
    prog (SharedLoc addr) =
      [ Load (sharedAddr addr) regB,
        Pop regA,
        WriteInstr regA (IndAddr regB)
      ]

-- Compile a single local declaration
--
-- Returns the new scope
compileLocalDecl :: Scope -> Decl -> (Scope, SprilProg)
compileLocalDecl scope (ident, expr) = (newScope, prog (lookupScopeLoc newScope ident))
  where
    ty = getExprType scope expr
    newScope = pushScopeVar scope ty ident
    prog (LocalLoc addr) =
      compileExpr scope expr
        ++ [ Pop regA,
             Store regA (ptr addr)
           ]
    prog (SharedLoc _) = error "Can't redeclare shared variable"

-- Compile a single expression
-- The result of the expression is always pushed to the stack.
compileExpr :: Scope -> Expr -> SprilProg
compileExpr scope (ParenExpr expr) =
  compileExpr scope expr
compileExpr scope (OpExpr op expr1 expr2) =
  compileOpExpr scope op expr1 expr2
compileExpr _ (ValueExpr value) =
  compileValueExpr value
compileExpr scope (IdentExpr ident) =
  compileIdentExpr scope ident
compileExpr scope (MethodExpr method) =
  compileMethod scope method

-- Pushes expr1, expr 2 on the stack, and then computes the operator and pushes
-- that back on the stack.
compileOpExpr :: Scope -> Op -> Expr -> Expr -> SprilProg
compileOpExpr scope op e1 e2
  | exprType == getOpExprType op (getExprType scope e1) (getExprType scope e2) =
    compileExpr scope e1
      ++ compileExpr scope e2
      ++ compileOp op exprType
  | otherwise = error "Can't compile op expression"
  where
    exprType = getOpExprType op (getExprType scope e1) (getExprType scope e2)

-- pushes the value of the method to the stack.
compileMethod :: Scope -> Method -> SprilProg --optional TODO: print "True" for 1 etc
compileMethod scope (PrintMethod expr) =
  compileExpr scope expr
    ++ [ Pop regA,
         WriteInstr regA numberIO,
         Push regA
       ]
compileMethod scope (SleepMethod expr) =
  compileExpr scope expr
    ++ [ Pop regA,
         Push regA,
         Store regB (ImmValue 1),

         Compute Lt regA regB regC,
         Branch regC (Rel 3), -- if bigger than 1
         Compute Sub regA regB regA, -- subtract
         Jump (Rel (-3))
       ]

-- Pushes the value of the identifier to the stack.
compileIdentExpr :: Scope -> Ident -> SprilProg
compileIdentExpr scope ident = prog (lookupScopeLoc scope ident)
  where
    prog (LocalLoc addr) =
      [ Load (ptr addr) regA,
        Push regA
      ]
    prog (SharedLoc addr) =
      [ Load (sharedAddr addr) regB,
        ReadInstr (IndAddr regB),
        Receive regA,
        Push regA
      ]

-- Pushes the immediate value to the stack
compileValueExpr :: ParseTree.Value -> SprilProg
compileValueExpr (IntValue int) =
  [ Load (ImmValue int) regA,
    Push regA
  ]
compileValueExpr (BoolValue bool) =
  [ Load (ImmValue (fromEnum bool)) regA,
    Push regA
  ]

-- Pops twice from the stack, and pushes the result back to the stack
compileOp :: Op -> Type -> SprilProg
compileOp AddOp _ =
  [ Pop regB,
    Pop regA,
    Compute Add regA regB regA,
    Push regA
  ]
compileOp SubOp _ =
  [ Pop regB,
    Pop regA,
    Compute Sub regA regB regA,
    Push regA
  ]
compileOp MulOp _ =
  [ Pop regB,
    Pop regA,
    Compute Mul regA regB regA,
    Push regA
  ]
compileOp GtOp _ =
  [ Pop regB,
    Pop regA,
    Compute Gt regA regB regA,
    Push regA
  ]
compileOp LtOp _ =
  [ Pop regB,
    Pop regA,
    Compute Lt regA regB regA,
    Push regA
  ]
compileOp EqOp _ =
  [ Pop regB,
    Pop regA,
    Compute Equal regA regB regA,
    Push regA
  ]
compileOp NeOp _ =
  [ Pop regB,
    Pop regA,
    Compute NEq regA regB regA,
    Push regA
  ]

-- Run with custom debug
runDebug :: [SprilProg] -> IO ()
runDebug = runWithDebugger (debuggerSimplePrint (\x -> myShow2 x))

myShow2 :: DbgInput -> String
myShow2 (instrs, s) =
  printf
    "instrs: %s - states: %s - shared %s"
    (show instrs)
    (unlines $ map show $ sprStates s)
    (show $ sharedMem s)

-- Helper function to take the right, or return an error.
takeRight :: Show a => Either a b -> b
takeRight (Right val) = val
takeRight (Left val) = error ("Could not parse: " ++ show val)

-- Get the address to the shared memory data
sharedAddr :: Int -> AddrImmDI
sharedAddr addr = ImmValue ((addr + 1) * 2)

-- Get the address to the shared memory lock
sharedLockAddr :: Int -> AddrImmDI
sharedLockAddr addr = ImmValue ((addr + 1) * 2 + 1)

-- Get the spawn-count shared memory address, used for synchronizing when a process is allowed to spawn.
scAddrSpawn :: SpawnCount -> Int
scAddrSpawn (SC sc) = (7 - (sc * 2))

-- Get the spawn-count shared memory address, used for synchronizing when a process exits.
scAddrExit :: SpawnCount -> Int
scAddrExit (SC sc) = (7 - (sc * 2) - 1)

-- helper function for creating a direct address.
ptr :: Int -> AddrImmDI
ptr num = DirAddr num
