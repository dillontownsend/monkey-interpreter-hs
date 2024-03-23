module Parser.AST where

newtype Program
    = Program [Statement]
    deriving (Eq, Show)

data Statement
    = LetStatement !Identifier -- !Expression
    | ReturnStatement -- Expression
    | ExpressionStatement !Expression
    deriving (Eq, Show)

newtype Identifier
    = Identifier String
    deriving (Eq, Show)

data Expression
    = IdentifierExpression !Identifier
    | IntegerLiteral !Int
    | PrefixExpression !PrefixOperator !Expression
    | InfixExpression !Expression !InfixOperator !Expression
    | BoolLiteral !Bool
    deriving (Eq, Show)

newtype ParserError
    = ParserError String
    deriving (Eq, Show)

data PrefixOperator
    = PrefixMinus
    | PrefixBang
    deriving (Eq, Show)

data InfixOperator
    = InfixPlus
    | InfixMinus
    | InfixMultiply
    | InfixDivide
    | InfixGreaterThan
    | InfixLessThan
    | InfixEqualTo
    | InfixNotEqualTo
    deriving (Eq, Show)
