{-# LANGUAGE OverloadedStrings #-}

module Parser (
    arith
    ) where

import Control.Applicative    hiding (many, (<|>))
import Control.Monad          (void)
import Prelude                hiding (pred, succ)
import Text.Parsec
import Text.Parsec.ByteString

import Core

--
-- Parser (not firm and needs to be fixed)
--

arith :: Parser [Command]
arith = separators >> endBy1 command termsep

-- command ::= term
command :: Parser Command
command = Eval <$> term

-- term ::= true | false | nat | succ | pred | if 
term :: Parser Term
term = true
    <|> false
    <|> nat
    <|> succ
    <|> pred
    <|> try iszero
    <|> ifTerm
    <|> surrounded

--
-- white space functions
--

comment :: Parser ()
comment = do
    void $ string "/*"
    void $ many (noneOf "*" <|> starNotCommentEnd)
    void commentEnd
    return ()

starNotCommentEnd :: Parser Char
starNotCommentEnd = try (many1 (char '*') *> noneOf "/") 

commentEnd :: Parser Char
commentEnd = many1 (char '*') *> char '/'

-- separator ::= space | comment
separator :: Parser ()
separator = void space <|> comment

separators :: Parser ()
separators = void $ many separator

separators1 :: Parser ()
separators1 = void $ many1 separator

-- termsep :: separator* ';' separator*
termsep :: Parser ()
termsep = separators >> char ';' >> separators

--
-- zero-arg term
--

-- true ::= 'true'
true :: Parser Term
true = string "true" *> pure TmTrue 

-- false ::= 'false'
false :: Parser Term
false = string "false" *> pure TmFalse

-- nat ::= ['0'-'9']+
nat :: Parser Term
nat = numToSucc . read <$> many1 digit
  where numToSucc 0 = TmZero
        numToSucc n = TmSucc $ numToSucc (n - 1 :: Integer)

--
-- one-arg term
--

-- succ ::= 'succ' term
succ :: Parser Term
succ = TmSucc <$> (string "succ" *> separators1 *> term)

-- pred ::= 'pred' term
pred :: Parser Term
pred = TmPred <$> (string "pred" *> separators1 *> term)

-- iszero :: 'iszero' term 
iszero :: Parser Term
iszero = TmIsZero <$> (string "iszero" *> separators1 *> term)

--
-- if term
--

-- if ::= 'if' term 'then' term 'else' term
ifTerm :: Parser Term
ifTerm = TmIf <$> (string "if" 
                 *> separators1
                 *> term
                 <* separators1)
              <*> (string "then"
                 *> separators1
                 *> term
                 <* separators1)
              <*> (string "else"
                 *> separators1
                 *> term)

-- surrounded ::= '(' term ')'
surrounded :: Parser Term
surrounded = char '(' *> term <* char ')'
