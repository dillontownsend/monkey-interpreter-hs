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
            \5 != 5;"
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
                                   ]

testOperatorPrecedenceParsing :: IO ()
testOperatorPrecedenceParsing =
    let
        input = "1 + 2 + 3;"
     in
        hspec $ do
            describe "parser" $ do
                it "operator precedence" $ do
                    let (Program ss) = P.parseProgram . P.new . L.new $ input
                    ss
                        `shouldBe` [ ExpressionStatement $ InfixExpression (InfixExpression (IntegerLiteral 1) InfixPlus (IntegerLiteral 2)) InfixPlus (IntegerLiteral 3)
                                   ]
