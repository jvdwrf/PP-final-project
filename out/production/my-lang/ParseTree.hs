{-# LANGUAGE FlexibleContexts #-}

module ParseTree where

import Data.Char (isAlphaNum, isLower)
import Text.Parsec
import Text.ParserCombinators.Parsec.Prim (Parser)
import Prelude hiding (lookup)

-- The parse-tree that we will end up with after parsing the raw code.
-- This contains 2 things:
-- + The shared code-block -> [Decl]
-- + And a bunch of root-statements -> [RootStat]
data ParseTree = ParseTree [Decl] [RootStat] deriving (Show, Eq)

-- A declaration: `let x = 10 - 1;`
type Decl = (Ident, Expr)

-- A piece of code that does not return anything.
data Stat
  = -- A declaration: `let x = 10 - 1;`
    DeclStat Decl
  | -- An Assignment: `x = 10 - 1;`
    AssignStat Ident Expr
  | -- An if statement: `if x == 10 { print(10) } else { print(5) }
    IfStat Expr [Stat] [Stat]
  | -- A while statement: `while x < 10 { x = x + 1; }
    WhileStat Expr [Stat]
  | -- A block statement: `{ x = 10; print(x); }`
    BlockStat [Stat]
  | -- An expression with semicolon: `10 + 10;`
    ExprStat Expr
  | -- An acquire statement: `acquire x { x = 10; }`
    AcquireStat Ident [Stat]
  deriving (Show, Eq)

-- A piece of code that returns a value, and thus has a type.
data Expr
  = -- An operator expression: `10 + 5`
    OpExpr Op Expr Expr
  | -- A parenthesized expression: `(x)`
    ParenExpr Expr
  | -- A method expression: `print(10)`
    MethodExpr Method
  | -- An identifier expression: `x`
    IdentExpr Ident
  | -- An immediate value expression: `10`
    ValueExpr Value
  deriving (Show, Eq)

-- An operator
data Op
  = -- Add: '+'
    AddOp
  | -- Subtract: '-'
    SubOp
  | -- Multiply: '*'
    MulOp
  | -- Greater than: '>'
    GtOp
  | -- Less than: '<'
    LtOp
  | -- Equal: '=='
    EqOp
  | -- Not equal: `!='
    NeOp
  deriving (Show, Eq)

-- A method
data Method
  = -- print an expression: `print(x)`
    PrintMethod Expr
  | -- sleep for x cycles: `sleep(10)`
    SleepMethod Expr
  deriving (Show, Eq)

-- An immediate value
data Value
  = -- An integer: `10`
    IntValue Int
  | -- A boolean: `True`
    BoolValue Bool
  deriving (Show, Eq)

-- A root-statement. This can be either a normal statement, or a spawn-statement
data RootStat
  = -- a normal statement: `let x = 10;
    RootStatStat Stat
  | -- A spawn statement: spawn { let x = 10; } do { let x = 11; }
    SpawnStat [RootStat] [RootStat]
  deriving (Show, Eq)

-- An identifier, which is just a string.
type Ident = String

-- Parses a string, and returns either a parse-tree, or an error
parseFML :: String -> Either ParseError ParseTree
parseFML = parse (wsP *> fmlP) ""

-- The entrypoint for parsing data into a parse-tree. This can compile an entire program, and will result
-- in error messages.
fmlP :: Parser ParseTree
fmlP = ParseTree <$> option [] (try sharedBlockP) <*> option [] (many rootStatP)

-- Parse a shared block of declarations
sharedBlockP :: Parser [Decl]
sharedBlockP =
  ( (stringP "shared" *> charP '{')
      *> many declP
  )
    <* charP '}'

-- Parse a single root statement
rootStatP :: Parser RootStat
rootStatP =
  try
    ( SpawnStat
        <$> ( (stringP "spawn" *> charP '{')
                *> many rootStatP
            )
        <* charP '}'
        <*> option [] (try (stringP "do" *> charP '{') *> (many rootStatP) <* charP '}')
    )
    <|> RootStatStat <$> statP

-- Parse a single declaration
declP :: Parser Decl
declP = (,) <$> ((stringP "let " *> identP) <* charP '=') <*> exprP <* charP ';'

-- Parse a single statement
statP :: Parser Stat
statP =
  DeclStat <$> declP
    <|> try
      ( IfStat <$> (stringP "if " *> exprP <* charP '{')
          <*> many statP
          <* charP '}'
          <*> option [] (try (stringP "else" *> charP '{') *> (many statP) <* charP '}')
      )
    <|> try
      ( WhileStat <$> (stringP "while " *> exprP <* charP '{')
          <*> many statP
          <* charP '}'
      )
    <|> (BlockStat <$> (charP '{' *> many statP) <* charP '}')
    <|> try (ExprStat <$> exprP <* charP ';')
    <|> try
      ( AcquireStat <$> (stringP "acquire " *> identP <* charP '{')
          <*> many statP
          <* charP '}'
      )
    <|> AssignStat <$> (identP <* charP '=') <*> (exprP <* charP ';')

-- Parse a single method
methodP :: Parser Method
methodP =
  try (PrintMethod <$> (stringP "print" *> charP '(' *> exprP) <* charP ')')
    <|> (SleepMethod <$> (stringP "sleep" *> charP '(' *> exprP) <* charP ')')

-- Parse a single expression
-- This has precedence level 0
exprP :: Parser Expr
exprP = chainl1 exprP' opP
  where
    opP =
      (OpExpr LtOp <$ charP '<')
        <|> (OpExpr GtOp <$ charP '>')
        <|> (OpExpr EqOp <$ stringP "==")
        <|> (OpExpr NeOp <$ stringP "!=")

-- Expression parser with precedence level 1
exprP' :: Parser Expr
exprP' = chainl1 exprP'' opP
  where
    opP =
      (OpExpr AddOp <$ charP '+')
        <|> (OpExpr SubOp <$ charP '-')

-- Expression parser with precedence level 2
exprP'' :: Parser Expr
exprP'' = chainl1 exprP''' opP
  where
    opP = (OpExpr MulOp <$ charP '*')

-- Expression parser with precedence level 3
exprP''' :: Parser Expr
exprP''' =
  ParenExpr <$> (charP '(' *> exprP) <* charP ')'
    <|> try (MethodExpr <$> methodP) -- MethodExpr (has overlap with identExpr)
    <|> ValueExpr <$> valueP -- ValueExpr (value and ident have no overlap)
    <|> IdentExpr <$> identP -- IdentExpr

-- Parse a single identifier
identP :: Parser Ident
identP = (:) <$> (satisfy isLower) <*> (many (satisfy isAlphaNum)) <* wsP

-- Parse a single immediate value
valueP :: Parser Value
valueP =
  try (BoolValue <$> boolP)
    <|> (IntValue <$> integerP)

-- Parse a single integer
integerP :: Parser Int
integerP = read <$> (many1 digit <* wsP)

-- Parse a single boolean
boolP :: Parser Bool
boolP = (True <$ stringP "True") <|> (False <$ stringP "False")

-- Helper for parsing strings with whitespace behind
stringP :: String -> Parser String
stringP s = string s <* wsP

-- Helper for parsing characters with whitespace behind
charP :: Char -> Parser Char
charP c = char c <* wsP

-- Helper for parsing whitespace and comments
wsP :: Parser ()
wsP = () <$ many (try (() <$ space) <|> commentP)

-- Parse a single-line comment: `let x = 10; // this is a comment \n x = x+1`
commentP :: Parser ()
commentP = () <$ (string "//" *> many (noneOf ['\n']))
