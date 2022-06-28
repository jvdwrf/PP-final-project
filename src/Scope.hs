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
  { sharedVars :: ScopeVars, -- Can only be declared once, and are copied over to a new scope as-is.
    localVars :: ScopeVars, -- Can be declared dynamically, and are copied over to a new scope as-is.
    stackPtr :: Int -- The place on the stack where the next variable will be set. Copied over to a new scope as-is.
  }
  deriving (Eq, Show)

-- Create a new root-scope, given all shared variables that have been declared.
-- This is used when a new process is spawned, so that it cannot access the local variables
-- of the other process.
newRootScope :: ScopeVars -> Scope
newRootScope shared =
  Scope
    { sharedVars = shared,
      localVars = Map.empty,
      stackPtr = 0
    }

-- Get the type of the (local or shared) ident.
-- If the ident does not exist, this will throw an error.
lookupScopeType :: Scope -> String -> Type
lookupScopeType scope ident
  | isJust maybeLocal = snd (fromJust maybeLocal)
  | isJust maybeShared = snd (fromJust maybeShared)
  | otherwise = error ("Variable " ++ ident ++ " not in scope.")
  where
    maybeLocal = Map.lookup ident (localVars scope)
    maybeShared = Map.lookup ident (sharedVars scope)

-- Get the location of the ident, either local or shared.
-- If the ident does not exist, this will throw an error.
lookupScopeLoc :: Scope -> String -> VarLoc
lookupScopeLoc scope ident
  | isJust maybeLocal = LocalLoc (fst (fromJust maybeLocal))
  | isJust maybeShared = SharedLoc (fst (fromJust maybeShared))
  | otherwise = error ("Variable " ++ ident ++ " not in scope.")
  where
    maybeLocal = Map.lookup ident (localVars scope)
    maybeShared = Map.lookup ident (sharedVars scope)

-- Push a new variable onto the current scope.
-- + If the variable overrides a shared variable, this will throw an error.
-- + If the variable overrides a local variable, the previous one will go out of scope.
-- + If the variable does not exist, it will be created.
pushScopeVar :: Scope -> Type -> String -> Scope -- Push a variable onto a scope
pushScopeVar scope ty ident
  | isJust (Map.lookup ident (sharedVars scope)) = error ("Cannot redeclare shared variable " ++ ident ++ ".")
  | otherwise =
    Scope
      { sharedVars = (sharedVars scope),
        localVars = Map.insert ident ((stackPtr scope), ty) (localVars scope),
        stackPtr = (stackPtr scope) + 1
      }
