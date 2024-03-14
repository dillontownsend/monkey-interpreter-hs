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
                    let (_, Program ss) = P.parseProgram . P.new . L.new $ input
                    length ss `shouldBe` 3
                it "expected statements" $ do
                    let (_, Program ss) = P.parseProgram . P.new . L.new $ input
                    map (\(LetStatement i) -> i) ss
                        `shouldBe` [ Identifier "x"
                                   , Identifier "y"
                                   , Identifier "foobar"
                                   ]
                it "no parser errors" $ do
                    let ((P.Parser _ _ _ es), _) = P.parseProgram . P.new . L.new $ input
                    length es `shouldBe` 0

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
                    let (_, Program ss) = P.parseProgram . P.new . L.new $ input
                    length ss `shouldBe` 3
                it "expected statements" $ do
                    let (_, Program ss) = P.parseProgram . P.new . L.new $ input
                    ss
                        `shouldBe` [ ReturnStatement
                                   , ReturnStatement
                                   , ReturnStatement
                                   ]
                it "no parser errors" $ do
                    let ((P.Parser _ _ _ es), _) = P.parseProgram . P.new . L.new $ input
                    length es `shouldBe` 0
