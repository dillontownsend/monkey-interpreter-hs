module Lexer.Test where

import Lexer.Lexer (lex)
import Lexer.Token
import Test.Hspec

code :: String
code =
    "let five = 5;\n\
    \let ten = 10;\n\
    \let add = fn(x, y) {\n\
    \  x + y;\n\
    \};\n\
    \let result = add (five, ten);\n\
    \!-/*5;\n\
    \5 < 10 > 5;"

code2 :: String
code2 =
    "if (5 < 10) {\n\
    \   return true;\n\
    \} else { \n\
    \   return false;\n\
    \}\n\
    \\n\
    \10 == 10;\n\
    \10 != 9;"

main :: IO ()
main = hspec $ do
    describe "lexer" $ do
        it "special chars" $ do
            Lexer.Lexer.lex "=+(){},;"
                `shouldBe` [ ASSIGN
                           , PLUS
                           , LPAREN
                           , RPAREN
                           , LBRACE
                           , RBRACE
                           , COMMA
                           , SEMICOLON
                           , EOF
                           ]
        it "complex code" $ do
            Lexer.Lexer.lex code
                `shouldBe` [ LET
                           , IDENT "five"
                           , ASSIGN
                           , INT 5
                           , SEMICOLON
                           , LET
                           , IDENT "ten"
                           , ASSIGN
                           , INT 10
                           , SEMICOLON
                           , LET
                           , IDENT "add"
                           , ASSIGN
                           , FUNCTION
                           , LPAREN
                           , IDENT "x"
                           , COMMA
                           , IDENT "y"
                           , RPAREN
                           , LBRACE
                           , IDENT "x"
                           , PLUS
                           , IDENT "y"
                           , SEMICOLON
                           , RBRACE
                           , SEMICOLON
                           , LET
                           , IDENT "result"
                           , ASSIGN
                           , IDENT "add"
                           , LPAREN
                           , IDENT "five"
                           , COMMA
                           , IDENT "ten"
                           , RPAREN
                           , SEMICOLON
                           , BANG
                           , MINUS
                           , SLASH
                           , ASTERISK
                           , INT 5
                           , SEMICOLON
                           , INT 5
                           , LESS_THAN
                           , INT 10
                           , GREATER_THAN
                           , INT 5
                           , SEMICOLON
                           , EOF
                           ]
        it "empty file" $ do
            Lexer.Lexer.lex "" `shouldBe` [EOF]
        it "complex code 2" $ do
            Lexer.Lexer.lex code2
                `shouldBe` [ IF
                           , LPAREN
                           , INT 5
                           , LESS_THAN
                           , INT 10
                           , RPAREN
                           , LBRACE
                           , RETURN
                           , BOOL True
                           , SEMICOLON
                           , RBRACE
                           , ELSE
                           , LBRACE
                           , RETURN
                           , BOOL False
                           , SEMICOLON
                           , RBRACE
                           , INT 10
                           , EQUAL_TO
                           , INT 10
                           , SEMICOLON
                           , INT 10
                           , NOT_EQUAL_TO
                           , INT 9
                           , SEMICOLON
                           , EOF
                           ]
