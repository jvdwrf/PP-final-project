{-# LANGUAGE FlexibleContexts #-}

module ParseTree where

import Data.Map (Map, lookup)
import Prelude hiding (lookup)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec.Char (char)
import Text.Parsec.Token (integer)
import Data.Functor.Identity (Identity)
import Data.Char (digitToInt, isLower, isLetter, isAlphaNum)
import Text.ParserCombinators.Parsec.Prim (Parser)
import Test.QuickCheck
import Control.Exception (evaluate)
import Text.Parsec
import Data.Either
import Text.Parsec.Expr (buildExpressionParser)

data ParseTree = ParseTree [Decl] [RootStat]  deriving (Show, Eq)

type Decl = (Ident, Expr)
type Process = [RootStat]

data Stat
  = DeclStat Decl
  | AssignStat Ident Expr
  | IfStat Expr [Stat] [Stat]
  | WhileStat Expr [Stat]
  | BlockStat [Stat]
  | ExprStat Expr
  | AcquireStat Ident [Stat] deriving (Show, Eq)

data Expr
  = OpExpr Op Expr Expr
  | ParenExpr Expr
  | MethodExpr Method
  | IdentExpr Ident
  | ValueExpr Value deriving (Show, Eq)

data Op = AddOp | SubOp | MulOp | GtOp | LtOp | EqOp deriving (Show, Eq)

data Method
  = GetMethod Expr Expr         --first expr: array; second expr: index
  | SetMethod Expr Expr Expr    --first expr: array; second expr: index; third expr: new value
  | PrintMethod Expr deriving (Show, Eq)

data Value
  = IntValue Int
  | BoolValue Bool
  | ArrayValue [Value] deriving (Show, Eq)

data RootStat
  = RootStatStat Stat
  | SpawnStat [RootStat] [RootStat] deriving (Show, Eq)

type Ident = String



fmlP :: Parser ParseTree
fmlP = ParseTree <$> option [] (try sharedBlockP) <*> option [] (many rootStatP)

sharedBlockP :: Parser [Decl]
sharedBlockP = ((stringP "shared" *> charP '{')
                             *> many declP)
                             <* charP '}'

rootStatP :: Parser RootStat
rootStatP = try (SpawnStat <$> ((stringP "spawn" *> charP '{')
              *> many rootStatP)
              <* charP '}'
              <*> option [] (try (stringP "do" *> charP '{') *> (many rootStatP) <* charP '}'))
             <|> RootStatStat <$> statP

declP :: Parser Decl
declP = (,) <$> ((stringP "let " *> identP) <* charP '=') <*> exprP <* charP ';'

--myParse statP "if x<6 {}"
statP :: Parser Stat
statP =  DeclStat <$> declP
        <|> try (IfStat <$> (stringP "if " *> exprP <* charP '{')
                      <*> many statP
                      <* charP '}'
                      <*> option [] (try (stringP "else" *> charP '{') *> (many statP) <* charP '}'))
        <|> try (WhileStat <$> (stringP "while " *> exprP <* charP '{')
                     <*> many statP
                     <* charP '}')
        <|> (BlockStat <$> (charP '{' *> many statP) <* charP '}')
        <|> try (ExprStat <$> exprP <* charP ';')
        <|> try (AcquireStat <$> (stringP "acquire " *> identP <* charP '{')
                     <*> many statP
                     <* charP '}')
        <|> AssignStat <$> (identP <* charP '=') <*> (exprP <* charP ';')


methodP :: Parser Method
methodP =
  GetMethod <$> (stringP "get" *> charP '(' *> exprP) <* charP ',' <*> exprP <* charP ')'
  <|> (SetMethod <$> (stringP "set" *> charP '(' *> exprP) <* charP ',' <*> exprP <* charP ',' <*> exprP <* charP ')')
  <|> (PrintMethod <$> (stringP "print" *> charP '(' *> exprP) <* charP ')')


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
exprP'''
--  = BlockExpr <$> (charP '{' *> many (try statP)) <*> exprP <* charP '}'
  = ParenExpr <$> (charP '(' *> exprP) <* charP ')'
  <|> try (MethodExpr <$> methodP)  -- MethodExpr (has overlap with identExpr)
  <|> ValueExpr <$> valueP          -- ValueExpr (value and ident have no overlap)
  <|> IdentExpr <$> identP          -- IdentExpr


identP :: Parser Ident --TODO: I added wsP to this!
identP = (:) <$> (satisfy isLower) <*> (many (satisfy isAlphaNum)) <* wsP

-- A parser that parses a single Value
valueP :: Parser Value
valueP =
  try (BoolValue <$> boolP)
  <|> (IntValue <$> integerP)
  <|> (ArrayValue <$> arrayP)

integerP :: Parser Int
integerP = read <$> (many1 digit <* wsP)

boolP :: Parser Bool
boolP =  (True <$ stringP "True") <|> (False <$ stringP "False")

arrayP :: Parser [Value]
arrayP =
  charP '['
  *> (
    (sepEndBy1 (BoolValue <$> boolP) (charP ','))
    <|> (sepEndBy1 (IntValue <$> integerP) (charP ','))
    <|> (sepEndBy1 (ArrayValue <$> arrayP) (charP ','))
  )
  <* charP ']'

stringP :: String -> Parser String
stringP s = string s <* wsP

charP :: Char -> Parser Char
charP c = char c <* wsP

wsP :: Parser ()
wsP =  () <$ many (() <$ space <|> commentP)

commentP :: Parser ()
commentP = () <$ (string "//" *> noneOf ['\n'])

myParse :: Parser a -> String -> Either ParseError a
myParse p = parse (wsP *> p) ""

parseFml :: String -> Either ParseError ParseTree
parseFml = parse (fmlP <* eof) ""


