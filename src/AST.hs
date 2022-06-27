module AST where

import Sprockell

var =
  [ [ Load (ImmValue 7) 3,
      TestAndSet (IndAddr 3),
      Receive 2,
      Branch 2 (Rel 4),
      Nop,
      Nop,
      Jump (Rel (-6)),

      Load (ImmValue 6) 3,
      TestAndSet (IndAddr 3),
      Receive 2,
      Branch 2 (Rel 2),
      Jump (Rel 13),
      WriteInstr 2 (ImmValue 0),
      Nop,
      Nop,
      Nop,
      Nop,
      Nop,
      Nop,
      Nop,
      Nop,
      Nop,
      Nop,
      Jump (Rel (-16)),

      EndProg
    ],
    [ Load (ImmValue 7) 3,
      TestAndSet (IndAddr 3),
      Receive 2,
      Branch 2 (Rel 2),
      Jump (Rel 13),
      WriteInstr 2 (ImmValue 0),
      Nop,
      Nop,
      Nop,
      Nop,
      Nop,
      Nop,
      Nop,
      Nop,
      Nop,
      Nop,
      Jump (Rel (-16)),

      Load (ImmValue 6) 3,
      TestAndSet (IndAddr 3),
      Receive 2,
      Branch 2 (Rel 4),
      Nop,
      Nop,
      Jump (Rel (-6)),

      EndProg
    ]
  ]

--
--import ParseTree
--import Data.Map (Map)
--import qualified Data.Map as Map
--import Scope
--import Compiler
--
--
---- ABSTRACT SYNTAX TREE
--
--data AST = AST [DeclNode] [RootStatNode]
--
--type DeclNode = (IdentNode, ExprNode)
--
--type IdentNode = (Ident, Scope)
--
--type ProcessNode = [RootStatNode]
--
--data RootStatNode
--  = RootStatStatNode StatNode
--  | SpawnStatNode [RootStatNode] [RootStatNode] SpawnCount
--  deriving (Show, Eq)
--
--data StatNode
--  = DeclStatNode DeclNode
--  | AssignStatNode IdentNode ExprNode
--  | IfStatNode ExprNode [StatNode] [StatNode]
--  | WhileStatNode ExprNode [StatNode]
--  | BlockStatNode [StatNode]
--  | ExprStatNode ExprNode
--  | AcquireStatNode IdentNode [StatNode]
--  deriving (Show, Eq)
--
--data ExprNode
--  = OpExprNode Op ExprNode ExprNode Type
--  | ParenExprNode ExprNode Type
--  | MethodExprNode MethodNode Type
--  | IdentExprNode Ident Type
--  | ValueExprNode Value Type
--  deriving (Show, Eq)
--
--data MethodNode
--  = GetMethodNode ExprNode ExprNode --first expr: array; second expr: index
--  | SetMethodNode ExprNode ExprNode Expr --first expr: array; second expr: index; third expr: new value
--  | PrintMethodNode ExprNode
--  deriving (Show, Eq)
--
--data ValueNode
--  = IntValueNode Int
--  | BoolValueNode Bool
--  | ArrayValueNode [ExprNode]
--  deriving (Show, Eq)
--
---- AST CREATION
--
--createAST :: ParseTree -> AST
--createAST (ParseTree decls rootstats) = AST sharedDeclNodes rootStatNodes
--  where
--    sharedDeclNodes = map createSharedDeclNode decls
--    sharedScope = sharedDeclScope sharedDeclNodes
--    (_scope', rootStatNodes) = createRootStatNodes sharedScope rootstats
--
--createSharedDeclNode :: Decl -> DeclNode
--createSharedDeclNode (ident, expr) = (ident, createExprNode newTypeScope expr)
--
--createIdentNode
--
----
--sharedDeclScope :: [DeclNode] -> Scope
--sharedDeclScope [] = newTypeScope
--sharedDeclScope ((ident, exprNode):declNodes) = addSharedVar scope ident (getExprType exprNode)
--  where
--    scope = sharedDeclScope declNodes
--
--createRootStatNodes :: Scope -> [RootStat] -> (Scope, [RootStatNode])
--createRootStatNodes scope [] = (scope, [])
--createRootStatNodes scope (rootStat:rootStats) = undefined
--  where
--
--
--createRootStatNode :: Scope -> RootStat -> (Scope, RootStatNode)
--createRootStatNode scope (RootStatStat stat) = (newScope, (RootStatStatNode statNode))
--  where
--    (newScope, statNode) = createStatNode scope stat
--createRootStatNode scope (SpawnStat spawnStats doStats) = (scope, SpawnStatNode spawnStatNodes doStatNodes)
--  where
--    (_, spawnStatNodes) = createRootStatNodes (removeLocalVars scope) spawnStats
--    (_, doStatNodes) = createRootStatNodes scope doStats
--
--
--createStatNode :: Scope -> Stat -> (Scope, StatNode)
--createStatNode scope stat = undefined
--
--getExprType :: ExprNode -> Type
--getExprType (OpExprNode _ _ _ ty) = ty
--getExprType (ParenExprNode _ ty) = ty
--getExprType (ValueExprNode _ ty) = ty
--getExprType (IdentExprNode _ ty) = ty
--getExprType (MethodExprNode _ ty) = ty
--
--createExprNode :: Scope -> Expr -> ExprNode
--createExprNode scope (OpExpr op expr1 expr2)
--  | (ty1 == ty2 && op == EqOp)
--      || (ty1 == ty2 && ty1 == IntType) =
--    OpExprNode op exprNode1 exprNode2 (getExprType exprNode2)
--  | otherwise = error ("can't operate between types")
--  where
--    exprNode1 = createExprNode scope expr1
--    exprNode2 = createExprNode scope expr2
--    ty1 = getExprType exprNode1
--    ty2 = getExprType exprNode2
--createExprNode scope (ParenExpr expr) = ParenExprNode exprNode (getExprType exprNode)
--  where
--    exprNode = createExprNode scope expr
--createExprNode scope (ValueExpr val) = ValueExprNode val (getValType undefined val)
--
--
--
--getValType :: Scope -> ParseTree.Value -> Type
--getValType _scope (IntValue _) = IntType
--getValType _scope (BoolValue _) = BoolType
----getValType scope (ArrayValue (t: ts)) = ArrayType (getExprType scope t) (length (t:ts))
--getValType _scope (ArrayValue _) = error "Can't create arrays with 0 elements"
--
--
