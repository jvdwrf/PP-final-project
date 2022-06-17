import Data.Either (isLeft)
import MyParser
import ParseTree
import Test.Hspec
import Test.QuickCheck
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepEndBy1)
import TypeChecking
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception.Base (evaluate)
import Text.Parsec.String (Parser)

main :: IO ()
main = hspec $ do
  describe "Boolp" $ do
    it "boolP works" $ do
      myParse boolP " False  " `shouldBe` Right False

    it "boolP works1" $ do
      myParse (boolP) "  True" `shouldBe` Right True

  describe "Wsp" $ do
    it "wsP works" $ do
      myParse wsP "   " `shouldBe` Right ()

    it "wsP works" $ do
      myParse wsP "" `shouldBe` Right ()

    it "wsP works2" $ do
      myParse wsP "  // ashjklas " `shouldBe` Right ()

    it "wsP works2" $ do
      myParse wsP "  [ " `shouldBe` Right ()

    it "wsP works2" $ do
      myParse (wsP *> char '[') "  [ " `shouldBe` Right '['

    it "wsP works2" $ do
      myParse (wsP *> charP '[') "  [ " `shouldBe` Right '['

  describe "charP" $ do
    it "charP works1" $ do
      myParse (charP '[') "  [ " `shouldBe` Right '['

  describe "integerP" $ do
    it "integerP works1" $ do
      myParse (wsP *> integerP) "  4567" `shouldBe` Right 4567

  describe "seperation parsing" $ do
    it "sepBy1 works1" $ do
      myParse (sepEndBy1 (charP 'a') (charP 'b')) "aba" `shouldBe` Right "aa"

    it "sepBy1 works1" $ do
      myParse (sepEndBy1 (charP 'a') (charP 'b')) " a  b a  b" `shouldBe` Right "aa"

    it "sepBy1 works1" $ do
      myParse (sepEndBy1 (charP 'a') (charP 'b')) " a  b a  b" `shouldBe` Right "aa"

    it "basic array is parsed" $ do
      myParse arrayP "[1,2]" `shouldBe` Right [IntValue 1, IntValue 2]

    it "basic array is parsed" $ do
      myParse valueP "  [1  ,2 ,]  " `shouldBe` Right (ArrayValue [IntValue 1, IntValue 2])

  describe "identifier parsing" $ do
    it "identP works1" $ do
      myParse (identP) "  myVar88" `shouldBe` Right "myVar88"
    it "identP works2" $ do
      myParse (identP) "  9myVar88" `shouldSatisfy` isLeft

  describe "method parsing" $ do
    it "get method works1" $ do
      myParse (methodP) "  get([5,7], 1)" `shouldBe` Right (GetMethod (ValueExpr (ArrayValue [IntValue 5, IntValue 7])) (ValueExpr (IntValue 1)))
    it "set method works1" $ do
      myParse (methodP) "  set([5,7], 1, 8)" `shouldBe` Right (SetMethod (ValueExpr (ArrayValue [IntValue 5, IntValue 7])) (ValueExpr (IntValue 1)) (ValueExpr (IntValue 8)))
    it "print method works1" $ do
      myParse (methodP) "  print(8)" `shouldBe` Right (PrintMethod (ValueExpr (IntValue 8)))

  describe "OpExpr parsing" $ do
    it "exprP works on less than" $ do
      myParse (exprP) "6<7" `shouldBe` Right (OpExpr LtOp (ValueExpr  (IntValue 6))   (ValueExpr (IntValue 7)))
    it "exprP works on greater than" $ do
      myParse (exprP) "12>70" `shouldBe` Right (OpExpr GtOp (ValueExpr (IntValue 12))  (ValueExpr (IntValue 70)))
    it "exprP works on equality" $ do
      myParse (exprP) "8==8" `shouldBe` Right (OpExpr EqOp (ValueExpr (IntValue 8))  (ValueExpr (IntValue 8)))
    it "exprP works on addition" $ do
      myParse (exprP) "5+8" `shouldBe` Right (OpExpr AddOp (ValueExpr (IntValue 5))  (ValueExpr (IntValue 8)))
    it "exprP works on subtraction" $ do
      myParse (exprP) "12-70" `shouldBe` Right (OpExpr SubOp (ValueExpr (IntValue 12))  (ValueExpr (IntValue 70)))
    it "exprP works on multiplication" $ do
      myParse (exprP) "7*6" `shouldBe` Right (OpExpr MulOp (ValueExpr (IntValue 7))  (ValueExpr (IntValue 6)))

  describe "BlockExpr parsing" $ do
    it "exprP works on a simple block with one statement" $ do
      myParse (exprP) "{let x=5; x}" `shouldBe` Right (BlockExpr [DeclStat ("x",ValueExpr (IntValue 5))] (IdentExpr "x"))
    it "exprP works on a simple block with multiple statements" $ do
      myParse (exprP) "{let x=5;\nlet y=7;\ny}" `shouldBe` Right (BlockExpr [DeclStat ("x",ValueExpr (IntValue 5)),DeclStat ("y",ValueExpr (IntValue 7))] (IdentExpr "y"))

  describe "Associativity parsing" $ do
    it "exprP works on a simple associativity #1" $ do
      myParse (exprP) "2+3*3"
      `shouldBe`
      Right (OpExpr AddOp (ValueExpr (IntValue 2)) (OpExpr MulOp (ValueExpr (IntValue 3)) (ValueExpr (IntValue 3))))
    it "exprP works on a simple associativity #2" $ do
      myParse (exprP) "5*2+3*3"
      `shouldBe`
      Right (OpExpr AddOp (OpExpr MulOp (ValueExpr (IntValue 5)) (ValueExpr (IntValue 2))) (OpExpr MulOp (ValueExpr (IntValue 3)) (ValueExpr (IntValue 3))))
    it "exprP works on a simple associativity #3" $ do
      myParse (exprP) "5-2+3*3"
      `shouldBe`
      Right (OpExpr AddOp (OpExpr SubOp (ValueExpr (IntValue 5)) (ValueExpr (IntValue 2))) (OpExpr MulOp (ValueExpr (IntValue 3)) (ValueExpr (IntValue 3))))
    it "exprP works on a simple associativity #5" $ do
      myParse (exprP) "1+1==1+1"
      `shouldBe`
      Right (OpExpr EqOp (OpExpr AddOp (ValueExpr (IntValue 1)) (ValueExpr (IntValue 1))) (OpExpr AddOp (ValueExpr (IntValue 1)) (ValueExpr (IntValue 1))))
    it "exprP works on a simple associativity #6" $ do
      myParse (exprP) "1<9>44"
      `shouldBe`
      Right (OpExpr GtOp (OpExpr LtOp (ValueExpr (IntValue 1)) (ValueExpr (IntValue 9))) (ValueExpr (IntValue 44)))

  describe "FML parsing" $ do
    it "fmlP works #1" $ do
      myParse (fmlP) " shared { let x = 7 ;\n let y = 4 ; } let d = 7 ; "
      `shouldBe`
      Right (ParseTree [("x",ValueExpr (IntValue 7)),("y", ValueExpr (  IntValue 4))] [RootStatStat (DeclStat ("d",ValueExpr ( IntValue 7)))])
    it "fmlP works with shared and acquire blocks #1" $ do
      myParse (fmlP) "shared{let x=7; let y=4;} d=7; acquire var { var=9; }"
      `shouldBe`
      Right (ParseTree [("x",ValueExpr ( IntValue 7)),("y",ValueExpr (IntValue 4))] [RootStatStat (AssignStat "d" (ValueExpr (IntValue 7))),RootStatStat (AcquireStat "var" [AssignStat "var" (ValueExpr (IntValue 9))])])
    it "fmlP works with simple spawn blocks #1" $ do
      myParse (fmlP) "spawn {  } do {  }"
      `shouldBe`
      Right (ParseTree [] [SpawnStat [] []])
    it "fmlP works with simple spawn blocks" $ do
      myParse (fmlP) "spawn { let x = 10; } do { let x = 10; }"
      `shouldBe`
      Right (ParseTree [] [SpawnStat [RootStatStat (DeclStat ("x",ValueExpr (IntValue 10)))] [RootStatStat (DeclStat ("x",ValueExpr (IntValue 10)))]])
    it "fmlP works with nested spawn blocks" $ do
      myParse (fmlP) "spawn { let x = 10; } do { spawn { let x = 10; } do { let x = 10; } }"
      `shouldBe`
      Right (ParseTree [] [SpawnStat [RootStatStat (DeclStat ("x",ValueExpr (IntValue 10)))] [SpawnStat [RootStatStat (DeclStat ("x",ValueExpr (IntValue 10)))] [RootStatStat (DeclStat ("x",ValueExpr (IntValue 10)))]]])
    it "can't spawn when not in root stat" $ do
      myParse (fmlP) "if (True) { }"
      `shouldBe`
      Right (ParseTree [] [RootStatStat (IfStat (ParenExpr (ValueExpr (BoolValue True))) [] [])])
    it "can't spawn when not in root stat" $ do
      myParse (fmlP) "spawn { }"
      `shouldBe`
      Right (ParseTree [] [SpawnStat [] []])
    it "can't spawn when not in root stat" $ do
      myParse (fmlP) "if (True) { spawn { } }"
      `shouldSatisfy` isLeft

  describe "type-checking" $ do
    it "basic-types" $ do
      getExprType (newRootScope Map.empty) (fromRight (myParse exprP "10")) `shouldBe` IntType
    it "basic-types" $ do
      getExprType (newRootScope Map.empty) (fromRight (myParse exprP "True")) `shouldBe` BoolType
    it "basic-types" $ do
      getExprType (newRootScope Map.empty) (fromRight (myParse exprP "[1,2]")) `shouldBe` ArrayType IntType 2
    it "basic-scope" $ do
      getExprType (testScope [] [("x", IntType)]) (fromRight (myParse exprP "x")) `shouldBe` IntType
    it "basic-scope" $ do
      getExprType (testScope [("x", (ValueExpr (IntValue 2)))] []) (fromRight (myParse exprP "x")) `shouldBe` IntType
    it "complex-types" $ do
      getExprType (testScope [] [("x", IntType)]) (fromRight (myParse exprP "x==2")) `shouldBe` BoolType
    it "complex-types" $ do
      evaluate (getExprType (testScope [] [("x", IntType)]) (fromRight (myParse exprP "x==True"))) `shouldThrow` anyException
    it "shared-decl to scope" $ do
      sharedDecl2Scope (testParse sharedBlockP "shared { let x = 1; }")
      `shouldBe`
      Scope { sharedVars = Map.fromList [("x", (0, IntType))], localVars = Map.empty, pushCount = 0, stackPtr = 0 }
    it "shared-decl to scope larger example" $ do
      sharedDecl2Scope (testParse sharedBlockP "shared { let y = [0,1]; let x = 1; }")
      `shouldBe`
      Scope { sharedVars = Map.fromList [("y", (0, (ArrayType IntType 2))), ("x", (2, IntType))], localVars = Map.empty, pushCount = 0, stackPtr = 0 }

testParse :: Parser b -> String -> b
testParse parser str = fromRight (myParse parser str)

testScope :: [(Decl)] -> [(String, Type)] -> Scope
testScope shared local = foldr (\(ident, ty) scope -> pushScopeVar scope ty ident) (sharedDecl2Scope shared) local

fromRight :: Either a b -> b
fromRight (Right v) = v
fromRight _ = error "Either was Left"