import Data.Either (isLeft)
import ParseTree
import Test.Hspec
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepEndBy1)
import Scope
import qualified Data.Map as Map
import Control.Exception.Base (evaluate)
import Text.Parsec.String (Parser)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (parse)
import TypeChecking

main :: IO ()
main = hspec $ do
  describe "bool parsing" $ do
    it "false" $ do
      myParse boolP " False  " `shouldBe` Right False

    it "true" $ do
      myParse (boolP) "  True" `shouldBe` Right True

  describe "value parsing" $ do
    it "int" $ do
      myParse valueP "10" `shouldBe` Right (IntValue 10)
    it "bool" $ do
      myParse valueP "True" `shouldBe` Right (BoolValue True)

  describe "whitespace parsing" $ do
    it "basic" $ do
      myParse wsP "   " `shouldBe` Right ()

    it "none" $ do
      myParse wsP "" `shouldBe` Right ()

    it "comment" $ do
      myParse wsP "  // ashjklas " `shouldBe` Right ()

    it "basic with followup" $ do
      myParse wsP "  [ " `shouldBe` Right ()

    it "whitespace with char" $ do
      myParse (wsP *> char '[') "  [ " `shouldBe` Right '['

  describe "char parsing" $ do
    it "basic" $ do
      myParse (charP '[') "  [ " `shouldBe` Right '['

  describe "integer parsing" $ do
    it "with whitespace" $ do
      myParse (wsP *> integerP) "  4567" `shouldBe` Right 4567

  describe "seperation parsing" $ do
    it "without end" $ do
      myParse (sepEndBy1 (charP 'a') (charP 'b')) "aba" `shouldBe` Right "aa"

    it "with end and whitespace" $ do
      myParse (sepEndBy1 (charP 'a') (charP 'b')) " a  b a  b" `shouldBe` Right "aa"


  describe "identifier parsing" $ do
    it "lowercase" $ do
      myParse (identP) "  myVar88" `shouldBe` Right "myVar88"
    it "number" $ do
      myParse (identP) "  9myVar88" `shouldSatisfy` isLeft

  describe "method parsing" $ do
    it "print" $ do
      myParse (methodP) "  print(8)" `shouldBe` Right (PrintMethod (ValueExpr (IntValue 8)))
    it "sleep" $ do
      myParse (methodP) "  sleep(8)" `shouldBe` Right (SleepMethod (ValueExpr (IntValue 8)))

  describe "operator-expression parsing" $ do
    it "lt" $ do
      myParse (exprP) "6<7" `shouldBe` Right (OpExpr LtOp (ValueExpr  (IntValue 6))   (ValueExpr (IntValue 7)))
    it "gt" $ do
      myParse (exprP) "12>70" `shouldBe` Right (OpExpr GtOp (ValueExpr (IntValue 12))  (ValueExpr (IntValue 70)))
    it "==" $ do
      myParse (exprP) "8==8" `shouldBe` Right (OpExpr EqOp (ValueExpr (IntValue 8))  (ValueExpr (IntValue 8)))
    it "+" $ do
      myParse (exprP) "5+8" `shouldBe` Right (OpExpr AddOp (ValueExpr (IntValue 5))  (ValueExpr (IntValue 8)))
    it "-" $ do
      myParse (exprP) "12-70" `shouldBe` Right (OpExpr SubOp (ValueExpr (IntValue 12))  (ValueExpr (IntValue 70)))
    it "*" $ do
      myParse (exprP) "7*6" `shouldBe` Right (OpExpr MulOp (ValueExpr (IntValue 7))  (ValueExpr (IntValue 6)))


  describe "associativity parsing" $ do
    it "#1" $ do
      myParse (exprP) "2+3*3"
      `shouldBe`
      Right (OpExpr AddOp (ValueExpr (IntValue 2)) (OpExpr MulOp (ValueExpr (IntValue 3)) (ValueExpr (IntValue 3))))
    it "#2" $ do
      myParse (exprP) "5*2+3*3"
      `shouldBe`
      Right (OpExpr AddOp (OpExpr MulOp (ValueExpr (IntValue 5)) (ValueExpr (IntValue 2))) (OpExpr MulOp (ValueExpr (IntValue 3)) (ValueExpr (IntValue 3))))
    it "#3" $ do
      myParse (exprP) "5-2+3*3"
      `shouldBe`
      Right (OpExpr AddOp (OpExpr SubOp (ValueExpr (IntValue 5)) (ValueExpr (IntValue 2))) (OpExpr MulOp (ValueExpr (IntValue 3)) (ValueExpr (IntValue 3))))
    it "#4" $ do
      myParse (exprP) "1+1==1+1"
      `shouldBe`
      Right (OpExpr EqOp (OpExpr AddOp (ValueExpr (IntValue 1)) (ValueExpr (IntValue 1))) (OpExpr AddOp (ValueExpr (IntValue 1)) (ValueExpr (IntValue 1))))
    it "#5" $ do
      myParse (exprP) "1<9>44"
      `shouldBe`
      Right (OpExpr GtOp (OpExpr LtOp (ValueExpr (IntValue 1)) (ValueExpr (IntValue 9))) (ValueExpr (IntValue 44)))

  describe "full language parsing" $ do
    it "shared variables" $ do
      myParse (fmlP) " shared { let x = 7 ;\n let y = 4 ; } let d = 7 ; "
      `shouldBe`
      Right (ParseTree [("x",ValueExpr (IntValue 7)),("y", ValueExpr (  IntValue 4))] [RootStatStat (DeclStat ("d",ValueExpr ( IntValue 7)))])
    it "shared and acquire blocks" $ do
      myParse (fmlP) "shared{let x=7; let y=4;} d=7; acquire var { var=9; }"
      `shouldBe`
      Right (ParseTree [("x",ValueExpr ( IntValue 7)),("y",ValueExpr (IntValue 4))] [RootStatStat (AssignStat "d" (ValueExpr (IntValue 7))),RootStatStat (AcquireStat "var" [AssignStat "var" (ValueExpr (IntValue 9))])])
    it "simple spawn blocks" $ do
      myParse (fmlP) "spawn {  } do {  }"
      `shouldBe`
      Right (ParseTree [] [SpawnStat [] []])
    it "complex spawn blocks" $ do
      myParse (fmlP) "spawn { let x = 10; } do { let x = 10; }"
      `shouldBe`
      Right (ParseTree [] [SpawnStat [RootStatStat (DeclStat ("x",ValueExpr (IntValue 10)))] [RootStatStat (DeclStat ("x",ValueExpr (IntValue 10)))]])
    it "nested spawn blocks" $ do
      myParse (fmlP) "spawn { let x = 10; } do { spawn { let x = 10; } do { let x = 10; } }"
      `shouldBe`
      Right (ParseTree [] [SpawnStat [RootStatStat (DeclStat ("x",ValueExpr (IntValue 10)))] [SpawnStat [RootStatStat (DeclStat ("x",ValueExpr (IntValue 10)))] [RootStatStat (DeclStat ("x",ValueExpr (IntValue 10)))]]])
    it "basic spawn" $ do
      myParse (fmlP) "spawn { }"
      `shouldBe`
      Right (ParseTree [] [SpawnStat [] []])
    it "can't spawn when not in root stat" $ do
      myParse (fmlP) "if (True) { spawn { } }"
      `shouldSatisfy` isLeft

  describe "type-checking for simple expressions" $ do
    it "basic-types1" $ do
      getExprType (newRootScope Map.empty) (fromRight (myParse exprP "10")) `shouldBe` IntType
    it "basic-types2" $ do
      getExprType (newRootScope Map.empty) (fromRight (myParse exprP "True")) `shouldBe` BoolType
    it "basic-scope5" $ do
      getExprType (testScope [] [("x", IntType)]) (fromRight (myParse exprP "x")) `shouldBe` IntType
    it "basic-scope6" $ do
      getExprType (testScope [("x", (ValueExpr (IntValue 2)))] []) (fromRight (myParse exprP "x")) `shouldBe` IntType
    it "complex-types" $ do
      getExprType (testScope [] [("x", IntType)]) (fromRight (myParse exprP "x==2")) `shouldBe` BoolType
    it "complex-types" $ do
      evaluate (getExprType (testScope [] [("x", IntType)]) (fromRight (myParse exprP "x==True"))) `shouldThrow` anyException

  describe "shared decl" $ do
    it "simple" $ do
      sharedDecl2Scope (testParse sharedBlockP "shared { let x = 1; }")
      `shouldBe`
      Scope { sharedVars = Map.fromList [("x", (0, IntType))], localVars = Map.empty, stackPtr = 0 }
    it "big" $ do
      sharedDecl2Scope (testParse sharedBlockP "shared { let y = True; let x = 1; }")
      `shouldBe`
      Scope { sharedVars = Map.fromList [("x",(1,IntType)),("y",(0,BoolType))], localVars = Map.empty, stackPtr = 0 }


testParse :: Parser b -> String -> b
testParse parser str = fromRight (myParse parser str)

testScope :: [(Decl)] -> [(String, Type)] -> Scope
testScope shared local = foldr (\(ident, ty) scope -> pushScopeVar scope ty ident) (sharedDecl2Scope shared) local

fromRight :: Either a b -> b
fromRight (Right v) = v
fromRight _ = error "Either was Left"

myParse :: Parser a -> String -> Either ParseError a
myParse p = parse (wsP *> p) ""