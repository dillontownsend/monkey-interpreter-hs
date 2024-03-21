module Parser.Parser where

import qualified Lexer.Lexer as L
import Lexer.Token
import Parser.AST

data Parser = Parser
    { lexer :: !L.Lexer
    , currToken :: !Token
    , peekToken :: !Token
    , parserErrors :: ![ParserError]
    }
    deriving (Show)

data Precedence
    = LOWEST
    | EQUALS
    | LESSGREATER
    | SUM
    | PRODUCT
    | PREFIX
    | CALL
    deriving (Eq, Show, Ord)

new :: L.Lexer -> Parser
new l =
    let (advancedLexer, token) = L.nextToken l
        (advancedLexer', token') = L.nextToken advancedLexer
     in Parser
            { lexer = advancedLexer'
            , currToken = token
            , peekToken = token'
            , parserErrors = []
            }

nextToken :: Parser -> Parser
nextToken parser =
    let (advancedLexer, token) = L.nextToken $ lexer parser
     in Parser
            { lexer = advancedLexer
            , currToken = peekToken parser
            , peekToken = token
            , parserErrors = parserErrors parser
            }

parseProgram :: Parser -> Program
parseProgram = Program . accumulateStatements []
  where
    accumulateStatements ss (Parser _ EOF _ _) = reverse ss
    accumulateStatements ss p =
        let (advancedParser, statement) = parseStatement p
            advancedParser' = nextToken advancedParser
         in case statement of
                Just s -> accumulateStatements (s : ss) advancedParser'
                Nothing -> accumulateStatements ss advancedParser'

parseStatement :: Parser -> (Parser, Maybe Statement)
parseStatement p = case currToken p of
    LET -> parseLetStatement p
    RETURN -> parseReturnStatement p
    _anyOtherToken -> parseExpressionStatement p

parseExpressionStatement :: Parser -> (Parser, Maybe Statement)
parseExpressionStatement parser =
    let (advancedParser, expression) = parseExpression parser LOWEST
     in (advanceToSemicolon advancedParser, Just $ ExpressionStatement expression)

parseExpression :: Parser -> Precedence -> (Parser, Expression)
parseExpression parser precendence =
    let (advancedParser, leftExpression) = parsePrefix parser
     in parseInfix advancedParser leftExpression precendence
  where
    parseInfix p@(Parser _ _ SEMICOLON _) e _ = (p, e)
    parseInfix p e prec =
        if prec >= peekPrecedence p
            then (p, e)
            else
                let infixParseFn = getInfixParseFn $ peekToken p
                 in case infixParseFn of
                        Just f ->
                            let (p', e') = f (nextToken p) e
                             in parseInfix p' e' prec
                        Nothing -> (p, e)
    parsePrefix p = case currToken p of
        IDENT _ -> (p, parseIdentifier p)
        INT _ -> (p, parseIntegerLiteral p)
        BANG -> parsePrefixExpression p
        MINUS -> parsePrefixExpression p
        _anyOtherToken -> error $ "cannot parse expression from currToken: " ++ (show _anyOtherToken)
    getInfixParseFn token = case token of
        PLUS -> Just parseInfixExpression
        MINUS -> Just parseInfixExpression
        SLASH -> Just parseInfixExpression
        ASTERISK -> Just parseInfixExpression
        EQUAL_TO -> Just parseInfixExpression
        NOT_EQUAL_TO -> Just parseInfixExpression
        LESS_THAN -> Just parseInfixExpression
        GREATER_THAN -> Just parseInfixExpression
        _anyOtherToken -> Nothing

parseInfixExpression :: Parser -> Expression -> (Parser, Expression)
parseInfixExpression parser left =
    let advancedParser = nextToken parser
        (advancedParser', right) = parseExpression advancedParser $ currPrecedence parser
     in ( advancedParser'
        , InfixExpression
            left
            ( case currToken parser of
                PLUS -> InfixPlus
                MINUS -> InfixMinus
                ASTERISK -> InfixMultiply
                SLASH -> InfixDivide
                GREATER_THAN -> InfixGreaterThan
                LESS_THAN -> InfixLessThan
                EQUAL_TO -> InfixEqualTo
                NOT_EQUAL_TO -> InfixNotEqualTo
                _anyOtherToken -> error $ "cannot get InfixOperator from token: " ++ show _anyOtherToken
            )
            right
        )

parseIdentifier :: Parser -> Expression
parseIdentifier (Parser _ (IDENT i) _ _) = IdentifierExpression $ Identifier i
parseIdentifier _ = error "currToken is not an IDENT"

parseIntegerLiteral :: Parser -> Expression
parseIntegerLiteral (Parser _ (INT i) _ _) = IntegerLiteral i
parseIntegerLiteral _ = error "currToken is not an INT"

parsePrefixExpression :: Parser -> (Parser, Expression)
parsePrefixExpression parser =
    let (advancedParser, expression) = parseExpression (nextToken parser) PREFIX
     in ( advancedParser
        , PrefixExpression
            ( case currToken parser of
                BANG -> PrefixBang
                MINUS -> PrefixMinus
                _anyOtherToken -> error "token is not a prefix operator"
            )
            expression
        )

parseLetStatement :: Parser -> (Parser, Maybe Statement)
parseLetStatement parser@(Parser _ _ (IDENT i) _) =
    let advancedParser = nextToken parser
     in case peekToken advancedParser of
            ASSIGN ->
                (advanceToSemicolon $ nextToken advancedParser, Just $ LetStatement $ Identifier i)
            _anyOtherToken -> (peekError advancedParser ASSIGN, Nothing)
parseLetStatement parser = (peekError parser $ IDENT "some_identifier", Nothing)

parseReturnStatement :: Parser -> (Parser, Maybe Statement)
parseReturnStatement parser = (advanceToSemicolon $ nextToken parser, Just ReturnStatement)

peekError :: Parser -> Token -> Parser
peekError parser expectedToken =
    Parser
        { lexer = lexer parser
        , currToken = currToken parser
        , peekToken = peekToken parser
        , parserErrors =
            ( ParserError $
                "expected next token to be "
                    ++ show expectedToken
                    ++ ", got "
                    ++ (show $ peekToken parser)
                    ++ " instead"
            )
                : parserErrors parser
        }

advanceToSemicolon :: Parser -> Parser
advanceToSemicolon p@(Parser _ SEMICOLON _ _) = p
advanceToSemicolon p = advanceToSemicolon $ nextToken p

getPrecedence :: Token -> Precedence
getPrecedence token = case token of
    EQUAL_TO -> EQUALS
    NOT_EQUAL_TO -> EQUALS
    LESS_THAN -> LESSGREATER
    GREATER_THAN -> LESSGREATER
    PLUS -> SUM
    MINUS -> SUM
    SLASH -> PRODUCT
    ASTERISK -> PRODUCT
    _anyOtherToken -> LOWEST

peekPrecedence :: Parser -> Precedence
peekPrecedence = getPrecedence . peekToken

currPrecedence :: Parser -> Precedence
currPrecedence = getPrecedence . currToken

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

main :: IO ()
main =
    readFile "app/main.monkey" >>= print . new . L.new
