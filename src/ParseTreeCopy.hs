{-# LANGUAGE FlexibleContexts #-}

module ParseTreeCopy
  () where

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

-- Parse Tree

type ParseTree = ([Decl], [RootStat])

type Decl = (Ident, Value) --TODO: change Value back to Expr

data RootStat
  = RootStatStat Stat
  | SpawnStat [RootStat] [RootStat] deriving (Show, Eq)



data Expr
  = OpExpr Expr Op Expr
  | ParenExpr Expr
  | BlockExpr [Stat] Expr
  | MethodExpr Method
  | IdentExpr Ident
  | ValueExpr Value deriving (Show, Eq)

data Stat
  = DeclStat Decl
  | AssignStat Ident Value  --TODO: chnage Value back to Expr
  | IfStat Value [Stat] [Stat] --TODO: chnage Value back to Expr
  | WhileStat Value [Stat] --TODO: change Value back to Expr
  | BlockStat [Stat]
  | ExprStat Value --TODO: change Value back to Expr
  | AcquireStat Ident [Stat] deriving (Show, Eq)

data Op = AddOp | SubOp | MulOp | GtOp | LtOp | EqOp deriving (Show, Eq)

data Method
  = GetMethod Expr Expr         --first expr: array; second expr: index
  | SetMethod Expr Expr Expr    --first expr: array; second expr: index; third expr: new value
  | PrintMethod Expr deriving (Show, Eq)

data Value
  = IntValue Int
  | BoolValue Bool
  | ArrayValue [Value] deriving (Show, Eq)

type Ident = String
--myParse statP "let y=5"
elsePartP :: Parser [Stat]
elsePartP = undefined

statP :: Parser Stat
statP =  (\x y -> DeclStat (x,y)) <$> ((stringP "let " *> identP) <* charP '=') <*> valueP <* charP ';'
          <|> try (IfStat <$> (stringP "if " *> valueP <* charP '{') --TODO: change valueP back to exprP
                        <*> many statP
                        <* charP '}'
                        <*> option [] ((stringP "else" *> charP '{') *> (many statP) <* charP '}'))
          <|> try (WhileStat <$> (stringP "while " *> valueP <* charP '{') --TODO: change valueP back to exprP
                       <*> many statP
                       <* charP '}')
          <|> (BlockStat <$> (charP '{' *> many statP) <* charP '}')
          <|> (ExprStat <$> valueP <* charP ';') --TODO: change valueP back to exprP
          <|> try (AcquireStat <$> (stringP "acquire " *> identP <* charP '{')
                       <*> many statP
                       <* charP '}')
          <|> AssignStat <$> (identP <* charP '=') <*> (valueP <* charP ';') --TODO: change valueP back to exprP


methodP :: Parser Method
methodP = GetMethod <$> (stringP "get" *> charP '(' *> exprP) <* charP ',' <*> exprP <* charP ')'
  <|> (SetMethod <$> (stringP "set" *> charP '(' *> exprP) <* charP ',' <*> exprP <* charP ',' <*> exprP <* charP ')')
  <|> (PrintMethod <$> (stringP "print" *> charP '(' *> exprP) <* charP ')')

exprP :: Parser Expr
exprP
  = BlockExpr <$> (charP '{' *> many statP) <*> exprP <* charP '}'
  <|> ParenExpr <$> (charP '(' *> exprP) <* charP ')'
  <|> (\(x, y, z) -> OpExpr x y z) <$> opExprP
  <|> try (MethodExpr <$> methodP)  -- MethodExpr (has overlap with identExpr)
  <|> ValueExpr <$> valueP          -- ValueExpr (value and ident have no overlap)
  <|> IdentExpr <$> identP          -- IdentExpr

opExprP :: Parser (Expr, Op, Expr)
opExprP = (\x y z -> (x, y, z)) <$> exprP <*> opP <*> exprP


opP :: Parser Op
opP
  = AddOp <$ charP '+'
  <|> SubOp <$ charP '-'
  <|> MulOp <$ charP '*'
  <|> GtOp <$ charP '>'
  <|> LtOp <$ charP '<'
  <|> EqOp <$ stringP "=="

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

------------------
-- HELPER TYPES --
------------------

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

----------
-- TEST --
----------

--test :: IO ()
--test = hspec $ do
--  describe "Boolp" $ do
--    it "boolP works" $ do
--      myParse boolP " False  " `shouldBe` Right False
--
--    it "boolP works1" $ do
--      myParse (boolP) "  True" `shouldBe` Right True
--
--  describe "Wsp" $ do
--    it "wsP works" $ do
--      myParse wsP "   " `shouldBe` Right ()
--
--    it "wsP works" $ do
--      myParse wsP "" `shouldBe` Right ()
--
--    it "wsP works2" $ do
--      myParse wsP "  // ashjklas " `shouldBe` Right ()
--
--    it "wsP works2" $ do
--      myParse wsP "  [ " `shouldBe` Right ()
--
--    it "wsP works2" $ do
--      myParse (wsP *> char '[') "  [ " `shouldBe` Right '['
--
--    it "wsP works2" $ do
--      myParse (wsP *> charP '[') "  [ " `shouldBe` Right '['
--
--  describe "charP" $ do
--    it "charP works1" $ do
--      myParse (charP '[') "  [ " `shouldBe` Right '['
--
--  describe "integerP" $ do
--
--    it "integerP works1" $ do
--      myParse (wsP *> integerP) "  4567" `shouldBe` Right 4567
--
--  describe "seperation parsing" $ do
--    it "sepBy1 works1" $ do
--      myParse (sepEndBy1 (charP 'a') (charP 'b')) "aba" `shouldBe` Right "aa"
--
--    it "sepBy1 works1" $ do
--      myParse (sepEndBy1 (charP 'a') (charP 'b')) " a  b a  b" `shouldBe` Right "aa"
--
--    it "sepBy1 works1" $ do
--      myParse (sepEndBy1 (charP 'a') (charP 'b')) " a  b a  b" `shouldBe` Right "aa"
--
--    it "basic array is parsed" $ do
--      myParse arrayP "[1,2]" `shouldBe` Right [IntValue 1, IntValue 2]
--
--    it "basic array is parsed" $ do
--      myParse valueP "  [1  ,2 ,]  " `shouldBe` Right (ArrayValue [IntValue 1, IntValue 2])
--
--  describe "identifier parsing" $ do
--    it "identP works1" $ do
--      myParse (identP) "  myVar88" `shouldBe` Right "myVar88"
--    it "identP works2" $ do
--      myParse (identP) "  9myVar88" `shouldSatisfy` isLeft
--
--  describe "method parsing" $ do
--    it "get method works1" $ do
--      myParse (methodP) "  get([5,7], 1)" `shouldBe` Right (GetMethod (ValueExpr (ArrayValue [IntValue 5, IntValue 7])) (ValueExpr (IntValue 1)))
--    it "set method works1" $ do
--      myParse (methodP) "  set([5,7], 1, 8)" `shouldBe` Right (SetMethod (ValueExpr (ArrayValue [IntValue 5, IntValue 7])) (ValueExpr (IntValue 1)) (ValueExpr (IntValue 8)))
--    it "print method works1" $ do
--      myParse (methodP) "  print(8)" `shouldBe` Right (PrintMethod (ValueExpr (IntValue 1)))
--
--
--  describe "OpExpr parsing" $ do
--      it "exprP works on less than" $ do
--        myParse (exprP) "6<7" `shouldBe` Right (OpExpr (ValueExpr (IntValue 1)) LtOp  (ValueExpr (IntValue 1)))
--      it "exprP works on greater than" $ do
--        myParse (exprP) "12>70" `shouldBe` Right (OpExpr (ValueExpr (IntValue 12)) GtOp  (ValueExpr (IntValue 70)))
--      it "exprP works on equality" $ do
--        myParse (exprP) "8==8" `shouldBe` Right (OpExpr (ValueExpr (IntValue 8)) EqOp  (ValueExpr (IntValue 8)))
--      it "exprP works on addition" $ do
--        myParse (exprP) "5+8" `shouldBe` Right (OpExpr (ValueExpr (IntValue 5)) GtOp  (ValueExpr (IntValue 8)))
--      it "exprP works on subtraction" $ do
--        myParse (exprP) "12-70" `shouldBe` Right (OpExpr (ValueExpr (IntValue 12)) SubOp  (ValueExpr (IntValue 70)))
--      it "exprP works on multiplication" $ do
--        myParse (exprP) "7*6" `shouldBe` Right (OpExpr (ValueExpr (IntValue 7)) GtOp  (ValueExpr (IntValue 6)))
--  describe "BlockExpr parsing" $ do
--        it "exprP works on a simple block with one statement" $ do
--          myParse (exprP) "{let x=5; x}" `shouldBe` Right (BlockExpr [DeclStat ("x", (ValueExpr (IntValue 5)))] (IdentExpr "x"))
--        it "exprP works on a simple block with multiple statement" $ do
--          myParse (exprP) "{let x=5;\nlet y=7;\ny}" `shouldBe` Right (BlockExpr [DeclStat ("x", (ValueExpr (IntValue 5))),DeclStat ("y", (ValueExpr (IntValue 7))) ] (IdentExpr "y"))



--data Expr
  --  = OpExpr Expr Op Expr
  --  | ParenExpr Expr
  --  | BlockExpr [Stat] Expr
  --  | MethodExpr Method
  --  | IdentExpr Ident
  --  | ValueExpr Value
--data Stat
  --  = DeclStat Decl
  --  | AssignStat Ident Expr
  --  | IfStat Expr [Stat] [Stat]
  --  | WhileStat Expr [Stat]
  --  | BlockStat [Stat]
  --  | ExprStat Expr
  --  | AcquireStat Ident [Stat] deriving (Show, Eq)
--type Decl = (Ident, Expr)



























-- AST
--type Dict = Map Ident Type
--
--data Type
--  = IntType
--  | BoolType
--  | ArrayType Type Int deriving (Eq, Show)
--
--getExprType :: Dict ->  Expr -> Type
--getExprType dict (OpExpr expr1 op expr2) = getOpExprType (getExprType dict expr1) op (getExprType dict expr2)
--getExprType dict (ParenExpr expr) = getExprType dict expr
--getExprType _dict (ValueExpr val) = getValType val
--getExprType dict (IdentExpr ident) = fromJust (lookup ident dict)
--getExprType dict (BlockExpr _ expr) = getExprType dict expr
--getExprType dict (MethodExpr method) = getMethodType dict method
--
--getOpExprType :: Type -> Op -> Type -> Type
--getOpExprType t1 EqOp t2
--  | t1 == t2 = BoolType
--  | otherwise = error "Can't compare two different types"
--getOpExprType t1 op t2
--  | t1 == t2 && t1 == IntType = getOpType op
--  | otherwise = error "Can't do operation on different types than int"
--
--getOpType :: Op -> Type
--getOpType EqOp = BoolType
--getOpType GtOp = BoolType
--getOpType LtOp = BoolType
--getOpType AddOp = IntType
--getOpType SubOp = IntType
--getOpType MulOp = IntType
--
--getValType :: Value -> Type
--getValType (IntValue _) = IntType
--getValType (BoolValue _) = BoolType
--getValType (ArrayValue (t: ts)) = ArrayType (getValType t) (length (t:ts))
--getValType (ArrayValue _) = error "Can't create arrays with 0 elements"
--
--getMethodType :: Dict -> Method -> Type
--getMethodType dict (GetMethod array _i) = getInnerArrayType (getExprType dict array)
--getMethodType dict (SetMethod array _i _val) = getInnerArrayType (getExprType dict array)
--getMethodType dict (PrintMethod expr) = getExprType dict expr
--
--getInnerArrayType :: Type -> Type
--getInnerArrayType (ArrayType t _len) = t
--getInnerArrayType _ = error "Is not of type array"
