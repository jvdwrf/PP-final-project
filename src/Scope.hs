module Scope where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data Type
  = IntType
  | BoolType
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
newRootScope shared =
  Scope
    { sharedVars = shared,
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
  | otherwise = pushScopeVar' scope ty ident
--  | otherwise = error ("Variable " ++ ident ++ " is declared multiple times within this scope.")
  where
    shared_var = Map.lookup ident (sharedVars scope)
    local_var = Map.lookup ident (localVars scope)

pushScopeVar' :: Scope -> Type -> String -> Scope
pushScopeVar' scope ty ident =
  Scope
    { sharedVars = (sharedVars scope),
      localVars = Map.insert ident ((stackPtr scope), ty) (localVars scope),
      pushCount = ((pushCount scope) + 1),
      stackPtr = (stackPtr scope) + 1
    }




