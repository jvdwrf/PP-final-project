module Scope where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import ParseTree
import Sprockell
import Sprockell (Instruction)

data Type
  = IntType
  | BoolType
--  | ArrayType Type Int
  deriving (Eq, Show)

-- A map from identifiers to their stackPtr and Type
type ScopeVars = Map String (Int, Type)

data VarLoc = LocalLoc Int | SharedLoc Int

data Scope = Scope
  { sharedVars :: ScopeVars,  -- Can only be declared once, and are copied over to a new scope as-is.
    localVars :: ScopeVars,   -- Can be declared dynamically, and are copied over to a new scope as-is.
    pushCount :: Int,         -- The amount of times a variable has been push in the current scope. set to 0 when opening a new scope.
    stackPtr :: Int           -- The place on the stack where the next variable will be set. Copied over to a new scope as-is.
  } deriving (Eq, Show)

-- Create a new root-scope, given all shared variables that have been declared
newRootScope :: ScopeVars -> Scope
newRootScope sharedVars =
  Scope
    { sharedVars = sharedVars,
      localVars = Map.empty,
      pushCount = 0,
      stackPtr = 0
    }

lookupScopeType :: Scope -> String -> Type
lookupScopeType scope ident
  | isJust maybeLocal = snd (fromJust maybeLocal)
  | isJust maybeShared = snd (fromJust maybeShared)
  | otherwise = error ("Variable " ++ ident ++ " not in scope.")
  where
    maybeLocal = Map.lookup ident (localVars scope)
    maybeShared = Map.lookup ident (sharedVars scope)

lookupScopeLoc :: Scope -> String -> VarLoc
lookupScopeLoc scope ident
  | isJust maybeLocal = LocalLoc (fst (fromJust maybeLocal))
  | isJust maybeShared = SharedLoc (fst (fromJust maybeShared))
  | otherwise = error ("Variable "++ident++" not in scope.")
  where
    maybeLocal = Map.lookup ident (localVars scope)
    maybeShared = Map.lookup ident (sharedVars scope)

openScope :: Scope -> Scope
openScope scope =
  Scope
    { sharedVars = (sharedVars scope),
      localVars = (localVars scope),
      pushCount = 0,
      stackPtr = (stackPtr scope)
    }

pushScopeVar :: Scope -> Type -> String -> Scope -- Push a variable onto a scope
pushScopeVar scope ty ident
  | isNothing shared_var && isNothing local_var = pushScopeVar' scope ty ident
  | isJust shared_var = error ("Cannot redeclare shared variable "++ident++".")
  | otherwise = error ("Variable " ++ ident ++ " is declared multiple times within this scope.")
  where
    shared_var = Map.lookup ident (sharedVars scope)
    local_var = Map.lookup ident (localVars scope)

pushScopeVar' :: Scope -> Type -> String -> Scope
pushScopeVar' scope ty ident =
  Scope
    { sharedVars = (sharedVars scope),
      localVars = Map.insert ident ((stackPtr scope), ty) (localVars scope),
      pushCount = ((pushCount scope) + size),
      stackPtr = (stackPtr scope) + size
    }
  where
    size = typeSize ty

sharedDecl2Scope :: [Decl] -> Scope
sharedDecl2Scope decls = newRootScope (Map.fromList (sharedDecl2List decls 0))

sharedDecl2List :: [Decl] -> Int -> [(String, (Int, Type))]
sharedDecl2List [] offset = []
sharedDecl2List ((ident, expr) : decls) offset = (ident, (offset, ty)) : sharedDecl2List decls (offset + (typeSize ty))
  where
    ty = getExprType (newRootScope Map.empty) expr

typeSize :: Type -> Int
typeSize IntType = 1
typeSize BoolType = 1


getExprType :: Scope ->  Expr -> Type
getExprType scope (OpExpr op expr1 expr2) = getOpExprType op (getExprType scope expr1)  (getExprType scope expr2)
getExprType scope (ParenExpr expr) = getExprType scope expr
getExprType scope (ValueExpr val) = getValType scope val
getExprType scope (IdentExpr ident) = (lookupScopeType scope ident)
--getExprType scope (BlockExpr _ expr) = getExprType scope expr
getExprType scope (MethodExpr method) = getMethodType scope method
--
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

--getInnerArrayType :: Type -> Type
--getInnerArrayType (ArrayType t _len) = t
--getInnerArrayType _ = error "Is not of type array"

getValType :: Scope -> ParseTree.Value -> Type
getValType _scope (IntValue _) = IntType
getValType _scope (BoolValue _) = BoolType
--getValType scope (ArrayValue (t: ts)) = ArrayType (getExprType scope t) (length (t:ts))
--getValType _scope (ArrayValue _) = error "Can't create arrays with 0 elements"