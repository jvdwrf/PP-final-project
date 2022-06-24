{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module ExParseExample () where


import Control.Applicative (some)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String

-- EXPR

data Expr
  = IntE Int        -- integer literals
  | FalseE | TrueE  -- boolean literals (F, T)
  | AddE Expr Expr  -- x + y
  | SubE Expr Expr  -- x - y
  | EqE  Expr Expr  -- x = y
  | OrE  Expr Expr  -- x | y
  | AndE Expr Expr  -- x & y
  deriving (Show)

-- VALUE

data Value = BoolV Bool | IntV Int deriving (Eq, Show)


op' :: Parser (Expr -> Expr -> Expr)
op' = AddE <$ char '+'
               <|> SubE <$ char '-'
               <|> EqE  <$ char '='
               <|> OrE  <$ char '|'
               <|> AndE <$ char '&'

-- WITHOUT PRECEDENCE
--
expr :: Parser Expr
expr = chainl1 term op
  where op = AddE <$ char '+'
         <|> SubE <$ char '-'
         <|> EqE  <$ char '='
         <|> OrE  <$ char '|'
         <|> AndE <$ char '&'
term :: Parser Expr
term = IntE . read <$> some digit
   <|> FalseE <$ char 'F' <|> TrueE <$ char 'T'
   <|> parens expr
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

eval :: Expr -> Value
eval (IntE x) = IntV x
eval FalseE = BoolV False
eval TrueE = BoolV True
eval (AddE e1 e2)
  = let IntV v1 = eval e1  -- pattern match ensures right type
        IntV v2 = eval e2
    in  IntV (v1 + v2)
eval (SubE e1 e2)
  = let IntV v1 = eval e1
        IntV v2 = eval e2
    in  IntV (v1 - v2)
eval (EqE e1 e2) = BoolV (eval e1 == eval e2)  -- equal if same type and value
eval (OrE e1 e2)
  = let BoolV v1 = eval e1
        BoolV v2 = eval e2
    in  BoolV (v1 || v2)
eval (AndE e1 e2)
  = let BoolV v1 = eval e1
        BoolV v2 = eval e2
    in  BoolV (v1 && v2)

evalExpr :: String -> Value
evalExpr str = let Right e = parse expr "<evalExpr>" str in eval e

-- WITH PRECEDENCE MANUAL

expr0 :: Parser Value
expr0 = chainl1 expr1 op
  where op = or  <$ char '|'
        or  (BoolV x) (BoolV y) = BoolV $ x || y

expr1 :: Parser Value
expr1 = chainl1 expr2 op
  where op = and <$ char '&'
        and (BoolV x) (BoolV y) = BoolV $ x && y
expr2 :: Parser Value
expr2 = chainl1 expr3 op
  where op = eq  <$ char '='
        eq  v1 v2 = BoolV $ v1 == v2
expr3 :: Parser Value
expr3 = chainl1 term'' op
  where op = add <$ char '+'  -- two operators at same precedence
         <|> sub <$ char '-'
        add (IntV x) (IntV y) = IntV $ x + y
        sub (IntV x) (IntV y) = IntV $ x - y


term'' :: Parser Value
term'' = IntV . read <$> some digit
     <|> BoolV False <$ char 'F' <|> BoolV True <$ char 'T'
     <|> parens expr0

-- WITH PRECEDENCE BUILT-IN

expr0' :: Parser Value
expr0' = buildExpressionParser table term''
  where table = [ [binary '+' add, binary '-' sub]
                , [binary '=' eq]
                , [binary '&' and]
                , [binary '|' or]
                ]
        binary c op = Infix (op <$ char c) AssocLeft
        add (IntV x) (IntV y) = IntV $ x + y
        sub (IntV x) (IntV y) = IntV $ x - y
        eq  v1 v2 = BoolV $ v1 == v2
        and (BoolV x) (BoolV y) = BoolV $ x && y
        or  (BoolV x) (BoolV y) = BoolV $ x || y
