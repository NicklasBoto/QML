module Frontend.Intermediate.Parser where

import Frontend.Intermediate.AST
import Frontend.Intermediate.Converter
import Frontend.QML.Par
import Frontend.QML.Layout
import Frontend.QML.ErrM
import Frontend.QML.Lex

ofParser :: (t -> p) -> ([Token] -> Err t) -> String -> p
ofParser conv par s = case par (resolveLayout True (myLexer s)) of
    Ok e -> conv e
    Bad s -> error s

parseExpr :: String -> Expr
parseExpr = convertExpr `ofParser` pExpr

parseType :: String -> Type
parseType = convertType `ofParser` pType

parseToplevel :: String -> Toplevel
parseToplevel = convertToplevel `ofParser` pToplevel

parse :: String -> Program
parse = convertProgram `ofParser` pProgram
