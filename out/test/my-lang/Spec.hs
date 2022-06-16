import Data.Either (isLeft)
import MyParser
import ParseTree
import Test.Hspec
import Test.QuickCheck
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepEndBy1)

main :: IO ()
main = hspec $ do
  describe "Parsing" $ do
    it "should parse numbers" $ do
      property $ \n -> (parseMyLang $ show (getPositive n)) `shouldBe` (Right (getPositive n) :: Either String Integer)

test :: IO ()
test = hspec $ do
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
      myParse (methodP) "  print(8)" `shouldBe` Right (PrintMethod (ValueExpr (IntValue 1)))

  describe "OpExpr parsing" $ do
    it "exprP works on less than" $ do
      myParse (exprP) "6<7" `shouldBe` Right (OpExpr (ValueExpr  (IntValue 1)) LtOp  (ValueExpr (IntValue 1)))
    it "exprP works on greater than" $ do
      myParse (exprP) "12>70" `shouldBe` Right (OpExpr (ValueExpr (IntValue 12)) GtOp (ValueExpr (IntValue 70)))
    it "exprP works on equality" $ do
      myParse (exprP) "8==8" `shouldBe` Right (OpExpr (ValueExpr (IntValue 8)) EqOp (ValueExpr (IntValue 8)))
    it "exprP works on addition" $ do
      myParse (exprP) "5+8" `shouldBe` Right (OpExpr (ValueExpr (IntValue 5)) GtOp (ValueExpr (IntValue 8)))
    it "exprP works on subtraction" $ do
      myParse (exprP) "12-70" `shouldBe` Right (OpExpr (ValueExpr (IntValue 12)) SubOp (ValueExpr (IntValue 70)))
    it "exprP works on multiplication" $ do
      myParse (exprP) "7*6" `shouldBe` Right (OpExpr (ValueExpr (IntValue 7)) GtOp (ValueExpr (IntValue 6)))
  describe "BlockExpr parsing" $ do
    it "exprP works on a simple block with one statement" $ do
      myParse (exprP) "{let x=5; x}" `shouldBe` Right undefined
    it "exprP works on a simple block with multiple statement" $ do
      myParse (exprP) "{let x=5;\nlet y=7;\ny}" `shouldBe` undefined
