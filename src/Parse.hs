{-# LANGUAGE FlexibleContexts #-}

module Parse where

import Text.Parsec

data Expr = Bind Char | Unit | Var Char | Lambda Expr Expr | App Expr Expr deriving (Show, Eq)

type Error = String

bind :: Stream s m Char => ParsecT s u m Expr
bind = do
        string "bind"
        many1 space
        l <- letter
        return (Bind l)

var :: Stream s m Char => ParsecT s u m Expr
var = do
        l <- letter
        return (Var l)

lambda :: Stream s m Char => ParsecT s u m Expr
lambda = do
        string "\\"
        v <- var
        string "."
        spaces
        e <- expr
        return (Lambda v e)

parens :: Stream s m Char => ParsecT s u m Expr -> ParsecT s u m Expr
parens p = do
        char '('
        e <- p
        char ')'
        return e

app :: Stream s m Char => ParsecT s u m (Expr->Expr->Expr)
app = do
        many1 space
        return App

unit :: Stream s m Char => ParsecT s u m Expr
unit = eof >> return Unit

atom :: Stream s m Char => ParsecT s u m Expr
atom = (unit <|> var <|> lambda) <|> parens atom

expr :: Stream s m Char => ParsecT s u m Expr
expr = chainl atom app Unit

parseLine :: Stream s m Char => ParsecT s u m Expr
parseLine = try (expr >>= \e -> eof >> return e) <|> (bind >>= \e -> eof >> return e)

parseExpr :: String -> Either ParseError Expr
parseExpr = parse parseLine ""