module Parser.Parser where

import qualified Lexer.Lexer as L
import Lexer.Token
import Parser.AST

data Parser = Parser
    { lexer :: !L.Lexer
    , currToken :: !Token
    , peekToken :: !Token
    , errors :: ![String]
    }
    deriving (Show)

new :: L.Lexer -> Parser
new l =
    let (advancedLexer, token) = L.nextToken l
        (advancedLexer', token') = L.nextToken advancedLexer
     in Parser advancedLexer' token token' []

nextToken :: Parser -> Parser
nextToken p =
    let (advancedLexer, token) = L.nextToken $ lexer p
     in Parser
            { lexer = advancedLexer
            , currToken = peekToken p
            , peekToken = token
            , errors = errors p
            }

parseProgram :: Parser -> (Parser, Program)
parseProgram parser@(Parser _ EOF _ _) = (parser, Program [])
parseProgram parser =
    let (parser', statement) = parseStatement parser
        parsed@(parser'', (Program ss)) = parseProgram $ nextToken parser'
     in case statement of
            Nothing -> parsed
            Just s -> (parser'', Program $ s : ss)

parseStatement :: Parser -> (Parser, Maybe Statement)
parseStatement parser = case currToken parser of
    LET -> parseLetStatement parser
    RETURN -> parseReturnStatement parser
    _anyOtherToken -> (parser, Nothing)

parseLetStatement :: Parser -> (Parser, Maybe Statement)
parseLetStatement parser@(Parser _ _ (IDENT s) _) =
    (advanceToSemicolon $ nextToken parser, Just $ LetStatement $ Identifier s)
parseLetStatement parser = (peekError parser $ IDENT "", Nothing)

advanceToSemicolon :: Parser -> Parser
advanceToSemicolon p@(Parser _ SEMICOLON _ _) = p
advanceToSemicolon p = advanceToSemicolon $ nextToken p

peekError :: Parser -> Token -> Parser
peekError parser expectedToken =
    Parser
        { lexer = lexer parser
        , currToken = currToken parser
        , peekToken = peekToken parser
        , errors =
            ("expected next token to be " ++ show expectedToken ++ ", got " ++ (show $ peekToken parser) ++ " instead") : errors parser
        }

parseReturnStatement :: Parser -> (Parser, Maybe Statement)
parseReturnStatement p = (advanceToSemicolon p, Just ReturnStatement)
