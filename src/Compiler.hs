module Compiler where

import Data.Map (Map)
import qualified Data.Map as Map
import ParseTree
import Sprockell
import TypeChecking

type SprilProg = [Instruction]
data SpawnCount = SC Int;

compile :: String -> [SprilProg]
compile str = map (\proc -> init ++ proc ++ end) processes
  where
    (ParseTree sharedDecl program) = takeRight (myParse fmlP str)
    takeRight (Right val) = val
    takeRight (Left val) = error ("Could not parse: " ++ show val)
    rootScope = sharedDecl2Scope sharedDecl
    init = compileSharedBlock (sharedVars rootScope) sharedDecl
    processes = compileProcess rootScope (SC 0) program
    end = [EndProg]


----- PROCESS -----
compileProcess :: Scope -> SpawnCount -> [RootStat] -> [SprilProg]
compileProcess _scope _sc [] = []
compileProcess scope sc ((RootStatStat stat):stats) = prog : compileProcess newScope sc stats
  where
    (newScope, prog) = compileStat scope stat
compileProcess scope (SC sc) ((SpawnStat spawnStats doStats):stats)
    = thisProgFull : spawnProgFull : doProgs ++ spawnProgs ++ compileProcess scope (SC sc) stats
  where
    spawnProg:spawnProgs = compileProcess (newRootScope (sharedVars scope)) (SC (sc + 1)) spawnStats
    spawnProgFull = beforeSpawnProg (SC sc) ++ spawnProg ++ awaitSpawnProg (SC sc)
    thisProg:doProgs = compileProcess scope (SC (sc + 1 + length spawnProgs)) doStats
    thisProgFull = beforeSpawnedProg (SC sc) ++ thisProg ++ spawnedExitProg (SC sc)


beforeSpawnProg :: SpawnCount -> SprilProg
beforeSpawnProg = undefined
beforeSpawnedProg :: SpawnCount -> SprilProg
beforeSpawnedProg = undefined
awaitSpawnProg :: SpawnCount -> SprilProg
awaitSpawnProg = undefined
spawnedExitProg :: SpawnCount -> SprilProg
spawnedExitProg = undefined


compileRootStats :: Scope -> [RootStat] -> SprilProg
compileRootStats scope stats = undefined

compileStat :: Scope -> Stat -> (Scope, SprilProg)
compileStat scope stat = undefined

----- SHARED ------

compileSharedBlock :: ScopeVars -> [Decl] -> SprilProg
compileSharedBlock vars sharedBlock = [] ++ compileSharedVarCreation (zip (Map.toList vars) sharedBlock) ++ []

compileSharedVarCreation :: [((String, (Int, Type)), Decl)] -> SprilProg
compileSharedVarCreation [] = []
compileSharedVarCreation (((ident, (ptr, ty)), (_ident, expr)) : decls) = instructions ++ compileSharedVarCreation decls
  where
    size = typeSize ty
    instructions =
      compileExpr (newRootScope Map.empty) expr -- result of expr is in regA
        ++ [
          WriteInstr regA (IndAddr i) | i <- [ ptr .. ptr + size ]
        ]

--      WriteReq
compileExprInto :: Scope -> Expr -> Int -> AddrImmDI -> SprilProg
compileExprInto scope expr size addr = undefined

-- Compiles an expression, and stores the result in regA
compileExpr :: Scope -> Expr -> SprilProg
compileExpr scope expr = undefined
