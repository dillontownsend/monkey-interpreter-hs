{-# LANGUAGE NamedFieldPuns #-}

module Lexer.Lexer where

import Data.Char (isAlpha, isDigit)
import Lexer.Token

data Lexer = Lexer
    { input :: !String
    , position :: !Int
    , readPosition :: !Int
    , ch :: !(Maybe Char)
    }
    deriving (Show)

new :: String -> Lexer
new s =
    readChar
        Lexer
            { input = s
            , position = 0
            , readPosition = 0
            , ch = Nothing
            }

readChar :: Lexer -> Lexer
readChar Lexer{input, readPosition} =
    let lexer = Lexer input readPosition $ readPosition + 1
        c =
            if readPosition >= length input
                then Nothing
                else Just $ input !! readPosition
     in lexer c

nextToken :: Lexer -> (Lexer, Token)
nextToken l =
    let lexer = skipWhitespace l
     in case ch lexer of
            Nothing -> (lexer, EOF)
            Just c ->
                case c of
                    '+' -> (readChar lexer, PLUS)
                    '-' -> (readChar lexer, MINUS)
                    '*' -> (readChar lexer, ASTERISK)
                    '/' -> (readChar lexer, SLASH)
                    '<' -> (readChar lexer, LESS_THAN)
                    '>' -> (readChar lexer, GREATER_THAN)
                    ';' -> (readChar lexer, SEMICOLON)
                    '(' -> (readChar lexer, LPAREN)
                    ')' -> (readChar lexer, RPAREN)
                    ',' -> (readChar lexer, COMMA)
                    '{' -> (readChar lexer, LBRACE)
                    '}' -> (readChar lexer, RBRACE)
                    '=' ->
                        if isPeekedCharEqual lexer
                            then (readChar $ readChar lexer, EQUAL_TO)
                            else (readChar lexer, ASSIGN)
                    '!' ->
                        if isPeekedCharEqual lexer
                            then (readChar $ readChar lexer, NOT_EQUAL_TO)
                            else (readChar lexer, BANG)
                    c'
                        | isLetter c' -> readIdentifier lexer
                        | isDigit c' -> readNumber lexer
                    _ -> (readChar lexer, ILLEGAL)

readIdentifier :: Lexer -> (Lexer, Token)
readIdentifier lexer =
    let start = position lexer
        advancedLexer = seek isLetter lexer
        end = position advancedLexer
        ident = take (end - start) $ drop start $ input lexer
     in (advancedLexer, lookupIdent ident)

isLetter :: Char -> Bool
isLetter c = c == '_' || isAlpha c

seek :: (Char -> Bool) -> Lexer -> Lexer
seek condition lexer@Lexer{ch = Just c}
    | condition c = seek condition $ readChar lexer
seek _ lexer = lexer

lookupIdent :: String -> Token
lookupIdent s = case s of
    "fn" -> FUNCTION
    "let" -> LET
    "true" -> BOOL True
    "false" -> BOOL False
    "if" -> IF
    "else" -> ELSE
    "return" -> RETURN
    _ -> IDENT s

skipWhitespace :: Lexer -> Lexer
skipWhitespace lexer@Lexer{ch = Just c}
    | c == ' ' || c == '\t' || c == '\n' || c == '\r' = skipWhitespace $ readChar lexer
skipWhitespace lexer = lexer

readNumber :: Lexer -> (Lexer, Token)
readNumber lexer =
    let start = position lexer
        advancedLexer = seek isDigit lexer
        end = position advancedLexer
        digit = take (end - start) $ drop start $ input lexer
     in (advancedLexer, INT $ read digit)

isPeekedCharEqual :: Lexer -> Bool
isPeekedCharEqual = (==) (Just '=') . peekChar
  where
    peekChar Lexer{input, readPosition} =
        if readPosition >= length input
            then Nothing
            else Just $ input !! readPosition

lex :: String -> [Token]
lex = accumulateTokens [] . new
  where
    accumulateTokens tokens@(EOF : _) _ = reverse tokens
    accumulateTokens tokens lexer =
        let (advancedLexer, token) = nextToken lexer
         in accumulateTokens (token : tokens) advancedLexer
