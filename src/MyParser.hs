module MyParser
    ( parseMyLang
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar)
import Text.Parsec.Combinator (many1)
import Control.Arrow (left)

num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)

parseNumUntilEnd :: String -> Either ParseError Integer
parseNumUntilEnd = parse (num <* eof) "Todo: filename"

parseMyLang s = left show $ parseNumUntilEnd s



-- Abstract Syntax Tree

type Prog = [Statement]

data Statement
  = LetStat Ident (Maybe Type) Expr
  | IfStat Expr [Statement] [Statement]
  | WhileStat Expr [Statement]
  | BlockStat [Statement]
  | ExprStat Expr

data Expr
  = OpExpr Expr Op Expr
  | ParenExpr Expr
  | BlockExpr [Statement] Expr
  | MethodExpr Method
  | IdentExpr Ident
  | Value Value
  
data Op = AddOp | SubOp | MulOp | GtOp | LtOp | EqOp

data Method
  = GetMethod Expr Expr         --first expr: array; second expr: index
  | SetMethod Expr Expr Expr    --first expr: array; second expr: index; third expr: new value
  | PrintMethod Expr

data Type
  = IntType
  | BoolType
  | ArrayType Type Integer      --type and size of array

data Value
  = IntValue Integer
  | BoolValue Bool
  | ArrayValue [Value]

type Ident = String
