module TypeChecking where

import Scope
import ParseTree
import qualified Data.Map as Map

getExprType :: Scope ->  Expr -> Type
getExprType scope (OpExpr op expr1 expr2) = getOpExprType op (getExprType scope expr1)  (getExprType scope expr2)
getExprType scope (ParenExpr expr) = getExprType scope expr
getExprType scope (ValueExpr val) = getValType scope val
getExprType scope (IdentExpr ident) = (lookupScopeType scope ident)
getExprType scope (MethodExpr method) = getMethodType scope method

getOpExprType :: Op -> Type -> Type -> Type
getOpExprType EqOp t1 t2
  | t1 == t2 = BoolType
  | otherwise = error "Can't compare two different types"
getOpExprType op t1 t2
  | t1 == t2 && t1 == IntType = getOpType op
  | otherwise = error "Can't do operation on different types than int"

getOpType :: Op -> Type
getOpType EqOp = BoolType
getOpType GtOp = BoolType
getOpType LtOp = BoolType
getOpType AddOp = IntType
getOpType SubOp = IntType
getOpType MulOp = IntType

getMethodType :: Scope -> Method -> Type
getMethodType scope (PrintMethod expr) = getExprType scope expr

getValType :: Scope -> ParseTree.Value -> Type
getValType _scope (IntValue _) = IntType
getValType _scope (BoolValue _) = BoolType

sharedDecl2Scope :: [Decl] -> Scope
sharedDecl2Scope decls = newRootScope (Map.fromList (sharedDecl2List decls 0))

sharedDecl2List :: [Decl] -> Int -> [(String, (Int, Type))]
sharedDecl2List [] offset = []
sharedDecl2List ((ident, expr) : decls) offset = (ident, (offset, ty)) : sharedDecl2List decls (offset + 1)
  where
    ty = getExprType (newRootScope Map.empty) expr