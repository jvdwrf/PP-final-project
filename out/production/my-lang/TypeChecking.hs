module TypeChecking where
import qualified Data.Map as Map
import ParseTree
import Scope

-- Get the type of an expression.
-- This will throw an error if any types used in this expression are incorrect.
getExprType :: Scope -> Expr -> Type
getExprType scope (OpExpr op expr1 expr2) = getOpExprType op (getExprType scope expr1) (getExprType scope expr2)
getExprType scope (ParenExpr expr) = getExprType scope expr
getExprType scope (ValueExpr val) = getValType scope val
getExprType scope (IdentExpr ident) = (lookupScopeType scope ident)
getExprType scope (MethodExpr method) = getMethodType scope method

-- Get the type of an Operator expression.
getOpExprType :: Op -> Type -> Type -> Type
getOpExprType EqOp t1 t2
  | t1 == t2 = BoolType -- If both are the same type, they can be compared as a boolean
  | otherwise = error "Can't compare (==) two different types"
getOpExprType NeOp t1 t2
  | t1 == t2 = BoolType -- If both are the same type, they can be compared as a boolean
  | otherwise = error "Can't compare (!=) two different types"
getOpExprType op t1 t2
  | t1 == t2 && t1 == IntType = getOpType op -- for all other operators, both must be integers
  | otherwise = error "Can't do operation on different types than int"

-- Get the type of an operator
getOpType :: Op -> Type
getOpType NeOp = BoolType
getOpType EqOp = BoolType
getOpType GtOp = BoolType
getOpType LtOp = BoolType
getOpType AddOp = IntType
getOpType SubOp = IntType
getOpType MulOp = IntType

-- Get the type of a method
getMethodType :: Scope -> Method -> Type
getMethodType scope (PrintMethod expr) = getExprType scope expr

-- Get the type of a value
getValType :: Scope -> ParseTree.Value -> Type
getValType _scope (IntValue _) = IntType
getValType _scope (BoolValue _) = BoolType

-- Create a scope from a bunch of declarations
sharedDecl2Scope :: [Decl] -> Scope
sharedDecl2Scope decls = newRootScope (Map.fromListWith (\_ _ -> error "Can't declare shared variable twice") (sharedDecl2List decls 0))

-- Create a list of variables from a root-scope
sharedDecl2List :: [Decl] -> Int -> [(String, (Int, Type))]
sharedDecl2List [] _ = []
sharedDecl2List ((ident, expr) : decls) offset = (ident, (offset, ty)) : sharedDecl2List decls (offset + 1)
  where
    ty = getExprType (newRootScope Map.empty) expr
