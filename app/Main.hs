module Main where

import Lexer.Lexer (lex)

main :: IO ()
main =
    readFile "app/main.monkey" >>= print . Lexer.Lexer.lex
