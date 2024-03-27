module Parser.Test where

import Test.Hspec

import qualified Lexer.Lexer as L
import Parser.AST
import qualified Parser.Parser as P

testLetStatement :: IO ()
testLetStatement =
    let
        input =
            "let x = 5;\n\
            \let y = 10;\n\
            \let foobar = 838383;"
     in
        hspec $ do
            describe "parser" $ do
                it "number of statements" $ do
                    let (Program ss) = P.parseProgram . P.new . L.new $ input
                    length ss `shouldBe` 3
                it "expected statements" $ do
                    let (Program ss) = P.parseProgram . P.new . L.new $ input
                    map (\(LetStatement i) -> i) ss
                        `shouldBe` [ Identifier "x"
                                   , Identifier "y"
                                   , Identifier "foobar"
                                   ]

testReturnStatement :: IO ()
testReturnStatement =
    let
        input =
            "return;\n\
            \return 10;\n\
            \return 2 + 2;"
     in
        hspec $ do
            describe "parser" $ do
                it "number of statements" $ do
                    let (Program ss) = P.parseProgram . P.new . L.new $ input
                    length ss `shouldBe` 3
                it "expected statements" $ do
                    let (Program ss) = P.parseProgram . P.new . L.new $ input
                    ss
                        `shouldBe` [ ReturnStatement
                                   , ReturnStatement
                                   , ReturnStatement
                                   ]

testExpressionStatement :: IO ()
testExpressionStatement =
    let
        input =
            "foobar;\n\
            \5;\n\
            \!foobar;\n\
            \-10;\n\
            \5 + 5;\n\
            \5 - 5;\n\
            \5 * 5;\n\
            \5 / 5;\n\
            \5 > 5;\n\
            \5 < 5;\n\
            \5 == 5;\n\
            \5 != 5;\n\
            \!false;"
     in
        hspec $ do
            describe "parser" $ do
                it "expected statements" $ do
                    let (Program ss) = P.parseProgram . P.new . L.new $ input
                    ss
                        `shouldBe` [ ExpressionStatement $ IdentifierExpression $ Identifier "foobar"
                                   , ExpressionStatement $ IntegerLiteral 5
                                   , ExpressionStatement $ PrefixExpression PrefixBang $ IdentifierExpression $ Identifier "foobar"
                                   , ExpressionStatement $ PrefixExpression PrefixMinus $ IntegerLiteral 10
                                   , ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixPlus (IntegerLiteral 5)
                                   , ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixMinus (IntegerLiteral 5)
                                   , ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixMultiply (IntegerLiteral 5)
                                   , ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixDivide (IntegerLiteral 5)
                                   , ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixGreaterThan (IntegerLiteral 5)
                                   , ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixLessThan (IntegerLiteral 5)
                                   , ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixEqualTo (IntegerLiteral 5)
                                   , ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixNotEqualTo (IntegerLiteral 5)
                                   , ExpressionStatement $ PrefixExpression PrefixBang $ BoolLiteral False
                                   ]

testOperatorPrecedenceParsing :: IO ()
testOperatorPrecedenceParsing =
    let
        input =
            "1 + 2 + 3;\n\
            \-1 + 2;\n\
            \(1 + 2) * 3;"
     in
        hspec $ do
            describe "parser" $ do
                it "operator precedence" $ do
                    let (Program ss) = P.parseProgram . P.new . L.new $ input
                    ss
                        `shouldBe` [ ExpressionStatement $ InfixExpression (InfixExpression (IntegerLiteral 1) InfixPlus (IntegerLiteral 2)) InfixPlus (IntegerLiteral 3)
                                   , ExpressionStatement $ InfixExpression (PrefixExpression PrefixMinus (IntegerLiteral 1)) InfixPlus (IntegerLiteral 2)
                                   , ExpressionStatement $ InfixExpression (InfixExpression (IntegerLiteral 1) InfixPlus (IntegerLiteral 2)) InfixMultiply (IntegerLiteral 3)
                                   ]

testIfExpression :: IO ()
testIfExpression =
    let
        input = "if (x < y) { x } else { y }" -- TODO - ELSE not parsing
     in
        hspec $ do
            describe "parser" $ do
                it "if expression" $ do
                    let (Program ss) = P.parseProgram . P.new . L.new $ input
                    ss
                        `shouldBe` [ ExpressionStatement $
                                        IfExpression
                                            { condition = InfixExpression (IdentifierExpression (Identifier "x")) InfixLessThan (IdentifierExpression (Identifier "y"))
                                            , consequence = Block $ [ExpressionStatement $ IdentifierExpression $ Identifier "x"]
                                            , alternative = Just $ Block $ [ExpressionStatement $ IdentifierExpression $ Identifier "y"]
                                            }
                                   ]

testFunctionLiteralParsing :: IO ()
testFunctionLiteralParsing =
    let
        input = "fn(x, y) { x + y; }"
     in
        hspec $ do
            describe "parser" $ do
                it "function literal" $ do
                    let (Program ss) = P.parseProgram . P.new . L.new $ input
                    ss
                        `shouldBe` [ ExpressionStatement $
                                        FunctionLiteral [Identifier "x", Identifier "y"] $
                                            Block
                                                [ ExpressionStatement $ InfixExpression (IdentifierExpression $ Identifier "x") InfixPlus (IdentifierExpression $ Identifier "y")
                                                ]
                                   ]
