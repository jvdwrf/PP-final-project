module Compiler where

--
import Data.Map (Map)
import qualified Data.Map as Map
import ParseTree
import Scope
import Sprockell
import Text.Printf (printf)



type SprilProg = [Instruction]

data SpawnCount = SC Int deriving (Show, Eq)

--
compile :: String -> [SprilProg]
compile str = map (\p -> initProcess ++ p ++ endProcess) (process : processes)
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
compileProcess scope sc ((RootStatStat stat) : stats) = (prog ++ restProg, restProgs)
  where
    (newScope, prog) = compileStat scope stat
    (restProg, restProgs) = compileProcess newScope sc stats
compileProcess scope (SC sc) ((SpawnStat spawnStats doStats) : stats) =
  (thisProgFull ++ restProg, spawnProgFull : doProgs ++ spawnProgs ++ restProgs)
  where
    (spawnProg, spawnProgs) = compileProcess (newRootScope (sharedVars scope)) (SC (sc + 1)) spawnStats
    spawnProgFull = beforeSpawnedProg (SC sc) ++ spawnProg ++ exitSpawnedProg (SC sc)
    (thisProg, doProgs) = compileProcess scope (SC (sc + 1 + length spawnProgs)) doStats
    thisProgFull = beforeSpawnProg (SC sc) ++ thisProg ++ awaitSpawnProg (SC sc)
    (restProg, restProgs) = compileProcess scope (SC sc) stats

-- Stat

compileStats :: Scope -> [Stat] -> (Scope, SprilProg)
compileStats scope [] = (scope, [])
compileStats scope (stat : stats) = (scope'', thisStatProg ++ restStatProg)
  where
    (scope', thisStatProg) = compileStat scope stat
    (scope'', restStatProg) = compileStats scope' stats

compileStat :: Scope -> Stat -> (Scope, SprilProg)
compileStat scope (DeclStat decl) = compileLocalDecl scope decl 
compileStat scope (ExprStat expr) = (scope, compileExpr scope expr ++ [Pop regA]) 
compileStat scope (AssignStat ident expr) = (scope, compileAssignStat scope ident expr) 
compileStat scope (IfStat condExpr ifStats elseStats) = (scope, compileIfStat scope condExpr ifStats elseStats) 
compileStat scope (WhileStat condExpr stats) = (scope, compileWhileStat scope condExpr stats) 
compileStat scope (BlockStat stats) = (scope, snd (compileStats scope stats))
compileStat scope (AcquireStat ident stats) = (scope, compileAcquireStat scope ident stats)

compileAcquireStat :: Scope -> Ident -> [Stat] -> SprilProg --TODO: check whether provided ident is actually a shared variable 
compileAcquireStat scope ident stats = undefined

compileWhileStat :: Scope -> Expr -> [Stat] -> SprilProg
compileWhileStat scope condExpr stats 
    | typeOfCond == BoolType
    = condProg -- now execute the condition
    ++ [
         Pop regA, -- and store it into regA
         Branch regA (Rel 2), -- branch depending on regA
         Jump (Rel (length prog + 2)) -- go here if true, and escape the loop
       ]
    ++ prog -- otherwise, execute the prog again
    ++ [
    Jump (Rel (-(length prog + 3 + length condProg)))
    ] -- and jump back to the top
    | otherwise = error ("While statement must have a condition of type BoolType but found "++show typeOfCond++".")
  where
    typeOfCond = getExprType scope condExpr
    (_, prog) = compileStats scope stats
    condProg = compileExpr scope condExpr


popEndScope :: Scope -> SprilProg
popEndScope scope = [(Pop regA) | _ <- [0..pushCount scope]]

compileIfStat :: Scope -> Expr -> [Stat] -> [Stat] -> SprilProg
compileIfStat scope condExpr ifStats elseStats
  | typeOfCond == BoolType
  = condProg
    ++ [ Pop regA,
         Branch regA (Rel (length elseProg + 2))
       ]
    ++ elseProg
    ++ [Jump (Rel (length ifProg + 1))]
    ++ ifProg
  | otherwise = error ("If statement must have a condition of type BoolType but found "++show typeOfCond++".")
  where
    typeOfCond = getExprType scope condExpr
    (_, ifProg) = compileStats scope ifStats
    (_, elseProg) = compileStats scope elseStats
    condProg = compileExpr scope condExpr

compileAssignStat :: Scope -> Ident -> Expr -> SprilProg
compileAssignStat scope ident expr
    | exprType == varType = exprProg ++ prog (lookupScopeLoc scope ident)
    | otherwise = error ("Cannot assign expression of type "++show exprType++" to variable "++ident++" of type "++show varType++".")
  where
    exprType = getExprType scope expr
    varType = lookupScopeType scope ident
    exprProg = compileExpr scope expr
    prog (LocalLoc addr) =
      [ Pop regA,
        Store regA (ptr addr)
      ]
    prog (SharedLoc addr) = undefined

compileLocalDecl :: Scope -> Decl -> (Scope, SprilProg)
compileLocalDecl scope (ident, expr) = (newScope, prog)
  where
    ty = getExprType scope expr
    newScope = pushScopeVar scope ty ident
    prog = compileExpr scope expr


compileExpr :: Scope -> Expr -> SprilProg
compileExpr scope (ParenExpr expr) =
  compileExpr scope expr
compileExpr scope (OpExpr op expr1 expr2) =
  compileExpr scope expr1
    ++ compileExpr scope expr2
    ++ compileOp op
compileExpr _ (ValueExpr value) =
  compileValueExpr value
compileExpr scope (IdentExpr ident) =
  compileIdentExpr scope ident
compileExpr scope (MethodExpr method) =
  compileMethod scope method

compileMethod :: Scope -> Method -> SprilProg --optional TODO: print "True" for 1 etc
compileMethod scope (PrintMethod expr) =
  compileExpr scope expr
    ++ [ Pop regA,
         WriteInstr regA numberIO,
         Push regA
       ]

compileIdentExpr :: Scope -> Ident -> SprilProg
compileIdentExpr scope ident = prog (lookupScopeLoc scope ident)
  where
    prog (LocalLoc addr) =
      [ Load (ptr addr) regA,
        Push regA
      ]
    prog (SharedLoc addr) = undefined

compileValueExpr :: ParseTree.Value -> SprilProg
compileValueExpr (IntValue int) =
  [ Load (ImmValue int) regA,
    Push regA
  ]
compileValueExpr (BoolValue bool) =
  [ Load (ImmValue (fromEnum bool)) regA,
    Push regA
  ]

compileOp :: Op -> SprilProg
compileOp AddOp =
  [ Pop regB,
    Pop regA,
    Compute Add regA regB regA,
    Push regA
  ]
compileOp SubOp =
  [ Pop regB,
    Pop regA,
    Compute Sub regA regB regA,
    Push regA
  ]
compileOp MulOp =
  [ Pop regB,
    Pop regA,
    Compute Mul regA regB regA,
    Push regA
  ]
compileOp GtOp =
  [ Pop regB,
    Pop regA,
    Compute Gt regA regB regA,
    Push regA
  ]
compileOp LtOp =
  [ Pop regB,
    Pop regA,
    Compute Lt regA regB regA,
    Push regA
  ]
compileOp EqOp =
  [ Pop regB,
    Pop regA,
    Compute Equal regA regB regA,
    Push regA
  ]

ptr :: Int -> AddrImmDI
ptr num = DirAddr num

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
compileSharedBlock _ _ = []

--compileSharedBlock :: ScopeVars -> [Decl] -> SprilProg
--compileSharedBlock vars sharedBlock = [] ++ compileSharedVarCreation (zip (Map.toList vars) sharedBlock) ++ []
--
--compileSharedVarCreation :: [((String, (Int, Type)), Decl)] -> SprilProg
--compileSharedVarCreation [] = []
--compileSharedVarCreation (((ident, (ptr, ty)), (_ident, expr)) : decls) = instructions ++ compileSharedVarCreation decls
--  where
--    size = typeSize ty
--    instructions =
--      compileExpr (newRootScope Map.empty) expr -- result of expr is in regA
--        ++ [ WriteInstr regA (IndAddr i) | i <- [ptr .. ptr + size]
--           ]

runDebug :: [SprilProg] -> IO ()
runDebug = runWithDebugger (debuggerSimplePrint (\x -> myShow2 x))

myShow2 :: DbgInput -> String
myShow2 (instrs,s) = printf "instrs: %s - states: %s" -- \nsprStates:\n%s\nrequests: %s\nreplies: %s\nrequestFifo: %s\nsharedMem: %s\n"
                    (show instrs)
                    (unlines $ map show $ sprStates s)
--                    (show $ requestChnls s)
--                    (show $ replyChnls s)
--                    (show $ requestFifo s)
--                    (show $ sharedMem s)