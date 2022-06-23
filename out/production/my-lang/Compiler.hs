module Compiler where

import Data.Map (Map)
import qualified Data.Map as Map
import ParseTree
import Sprockell
import TypeChecking

type SprilProg = [Instruction]
data SpawnCount = SC Int;

compile :: String -> [SprilProg]
compile str = map (\p -> initProcess ++ p ++ endProcess) (process:processes)
  where
    (ParseTree sharedDecl program) = takeRight (myParse fmlP str)
    takeRight (Right val) = val
    takeRight (Left val) = error ("Could not parse: " ++ show val)
    rootScope = sharedDecl2Scope sharedDecl
    initProcess = compileSharedBlock (sharedVars rootScope) sharedDecl
    endProcess = [EndProg]
    (process, processes) = compileProcess rootScope (SC 0) program


----- PROCESS -----
compileProcess :: Scope -> SpawnCount -> [RootStat] -> (SprilProg, [SprilProg])
compileProcess _scope _sc [] = ([], [])

compileProcess scope sc ((RootStatStat stat):stats) = (prog ++ restProg, restProgs)
  where
    (newScope, prog) = compileStat scope stat
    (restProg, restProgs) = compileProcess newScope sc stats

compileProcess scope (SC sc) ((SpawnStat spawnStats doStats):stats)
    = (thisProgFull ++ restProg, spawnProgFull : doProgs ++ spawnProgs ++ restProgs)
  where
    (spawnProg, spawnProgs) = compileProcess (newRootScope (sharedVars scope)) (SC (sc + 1)) spawnStats
    spawnProgFull = beforeSpawnedProg (SC sc) ++ spawnProg ++ exitSpawnedProg (SC sc)
    (thisProg, doProgs) = compileProcess scope (SC (sc + 1 + length spawnProgs)) doStats
    thisProgFull = beforeSpawnProg (SC sc) ++ thisProg ++ awaitSpawnProg (SC sc)
    (restProg, restProgs) = compileProcess scope (SC sc) stats


-- Stat

compileStat :: Scope -> Stat -> (Scope, SprilProg)
compileStat scope (DeclStat decl) = compileDecl scope decl


compileDecl :: Scope -> Decl -> (Scope, SprilProg)
compileDecl scope (ident, expr) = (newScope, prog)
  where
    compiledExpr = compileExpr scope expr
    newScope = undefined
    prog = undefined


compileExpr :: Scope -> Expr -> SprilProg
compileExpr scope (ParenExpr expr) = compileExpr scope expr
compileExpr scope (OpExpr op expr1 expr2)
    = compileExpr scope expr1
    ++ compileExpr scope expr2
    ++ op2iloc op

op2iloc :: Op -> SprilProg
op2iloc (Op AddOp) = []

----- SHARED ------

beforeSpawnProg :: SpawnCount -> SprilProg
beforeSpawnProg = undefined
beforeSpawnedProg :: SpawnCount -> SprilProg
beforeSpawnedProg = undefined
awaitSpawnProg :: SpawnCount -> SprilProg
awaitSpawnProg = undefined
exitSpawnedProg :: SpawnCount -> SprilProg
exitSpawnedProg = undefined

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


