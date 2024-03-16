module Parser.AST where

import Lexer.Token

data Program
    = Program [Statement]
    deriving (Eq, Show)

data Statement
    = LetStatement !Identifier -- !Expression
    | ReturnStatement -- !Expression
    | ExpressionStatement !Token !Expression
    deriving (Eq, Show)

data Expression
    = IdentifierExpression !Identifier
    deriving (Eq, Show)

data Identifier
    = Identifier String
    deriving (Eq, Show)
