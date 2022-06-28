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
    -- Subtract: '-'
  | SubOp
    -- Multiply: '*'
  | MulOp
    -- Greater than: '>'
  | GtOp
    -- Less than: '<'
  | LtOp
    -- Equal: '=='
  | EqOp
    -- Not equal: `-='
  | NeOp
  deriving (Show, Eq)

-- A method
data Method
  = PrintMethod Expr
  deriving (Show, Eq)

-- An immediate value: `10`
data Value
  = IntValue Int
  | BoolValue Bool
  deriving (Show, Eq)

-- A root-statement. This can be either
data RootStat
  = RootStatStat Stat
  | SpawnStat [RootStat] [RootStat]
  deriving (Show, Eq)

type Ident = String

fmlP :: Parser ParseTree
fmlP = ParseTree <$> option [] (try sharedBlockP) <*> option [] (many rootStatP)

sharedBlockP :: Parser [Decl]
sharedBlockP =
  ( (stringP "shared" *> charP '{')
      *> many declP
  )
    <* charP '}'

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

declP :: Parser Decl
declP = (,) <$> ((stringP "let " *> identP) <* charP '=') <*> exprP <* charP ';'

--myParse statP "if x<6 {}"
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

methodP :: Parser Method
methodP = (PrintMethod <$> (stringP "print" *> charP '(' *> exprP) <* charP ')')

exprP :: Parser Expr
exprP = chainl1 exprP' opP
  where
    opP =
      (OpExpr LtOp <$ charP '<')
        <|> (OpExpr GtOp <$ charP '>')
        <|> (OpExpr EqOp <$ stringP "==")

exprP' :: Parser Expr
exprP' = chainl1 exprP'' opP
  where
    opP =
      (OpExpr AddOp <$ charP '+')
        <|> (OpExpr SubOp <$ charP '-')

exprP'' :: Parser Expr
exprP'' = chainl1 exprP''' opP
  where
    opP = (OpExpr MulOp <$ charP '*')

exprP''' :: Parser Expr
exprP''' =
  --  = BlockExpr <$> (charP '{' *> many (try statP)) <*> exprP <* charP '}'
  ParenExpr <$> (charP '(' *> exprP) <* charP ')'
    <|> try (MethodExpr <$> methodP) -- MethodExpr (has overlap with identExpr)
    <|> ValueExpr <$> valueP -- ValueExpr (value and ident have no overlap)
    <|> IdentExpr <$> identP -- IdentExpr

identP :: Parser Ident --TODO: I added wsP to this!
identP = (:) <$> (satisfy isLower) <*> (many (satisfy isAlphaNum)) <* wsP

-- A parser that parses a single Value
valueP :: Parser Value
valueP =
  try (BoolValue <$> boolP)
    <|> (IntValue <$> integerP)

integerP :: Parser Int
integerP = read <$> (many1 digit <* wsP)

boolP :: Parser Bool
boolP = (True <$ stringP "True") <|> (False <$ stringP "False")

stringP :: String -> Parser String
stringP s = string s <* wsP

charP :: Char -> Parser Char
charP c = char c <* wsP

wsP :: Parser ()
wsP = () <$ many (() <$ space <|> commentP)

commentP :: Parser ()
commentP = () <$ (string "//" *> noneOf ['\n'])

myParse :: Parser a -> String -> Either ParseError a
myParse p = parse (wsP *> p) ""

parseFml :: String -> Either ParseError ParseTree
parseFml = parse (fmlP <* eof) ""
