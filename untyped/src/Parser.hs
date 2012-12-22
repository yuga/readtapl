{-# LANGUAGE OverloadedStrings #-}

module Parser (
    untyped
    ) where

import Control.Applicative    hiding (many, (<|>))
import Control.Monad          (void)
import Core
import Data.ByteString        (ByteString)
import Prelude                hiding (pred, succ)
import Text.Parsec

-- import Debug.Trace

--
-- Define Parser
--

type Parser = Parsec ByteString Context

--
-- Parser (not firm and needs to be fixed)
--

untyped :: Parser [Command]
untyped = separators >> endBy1 command termsep 

command :: Parser Command
command = commandBind <|> commandTerm 

commandTerm :: Parser Command
commandTerm = Eval <$> term

commandBind :: Parser Command
commandBind = uncurry Bind <$> binding

binding :: Parser (String, Binding)
binding = try $ do
    x <- lcid <* char '/'
    ctx <- getState
    setState $ appendNameBind ctx x
    return (x, NameBind)

-- --------------
-- Tokens
-- --------------

lambda :: Parser ()
lambda = reserved "lambda"

lcid :: Parser String
lcid = identifier

uscore :: Parser () 
uscore = reserved "_"

dot :: Parser ()
dot = void $ lexeme $ char '.'

--
-- Identifier and Reserved words
--

symbol :: String -> Parser String
symbol name = lexeme $ string name

reserved :: String -> Parser ()
reserved name = lexeme $ try $ do
    void $ string name
    notFollowedBy identLetter <?> "end of " ++ show name

identifier :: Parser String
identifier = lexeme $ try $ do
    name <- ident
    if isReserved name then
        unexpected ("reserved word: " ++ show name)
     else
        return name

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    separators
    return x

ident :: Parser String
ident = do
    { c <- identStart
    ; cs <- identLetter
    ; return $ c:cs }
    <?> "identifier"

identStart :: Parser Char
identStart = lower

identLetter :: Parser String
identLetter = many lower

isReserved :: String -> Bool
isReserved name = "lambda" == name

--
-- White Space
--

-- comment := '/*' string '*/'
comment :: Parser ()
comment = try $ do
    void $ string "/*"
    void $ many (noneOf "*" <|> starNotCommentEnd)
    void $ commentEnd
    return ()

starNotCommentEnd :: Parser Char
starNotCommentEnd = try (many1 (char '*') *> noneOf "/")

commentEnd :: Parser Char
commentEnd = many1 (char '*') *> char '/'

-- separator := space | comment
separator :: Parser ()
separator = (void space) <|> comment

separators :: Parser ()
separators = void $ many separator

--separators1 :: Parser ()
--separators1 = void $ many1 separator

-- termsep :: separator* ';' separator*
termsep :: Parser ()
termsep = separators >> char ';' >> separators

-- --------------
-- Terms
-- --------------

-- term := appTerm | tmAbs
term :: Parser Term
term = appTerm <|> tmAbs

-- appTerm := aTerm | tmApp
appTerm :: Parser Term
appTerm = chainl1 aTerm tmApp

-- aTerm := surrounded | tmVar
aTerm :: Parser Term
aTerm = surrounded <|> tmVar

-- tmAbs := 'lambda' lcid '.' Term | 'lambda' '_' 
tmAbs :: Parser Term
tmAbs = lcidAbs <|> uscoreAbs
  where
    lcidAbs = do
        x <- try lambda *> lcid <* dot
        ctx <- getState
        setState $ appendNameBind ctx x
        t <- term
        setState ctx
        return $ TmAbs x t
    uscoreAbs = do
        lambda >> uscore >> dot
        ctx <- getState
        setState $ appendNameBind ctx "_"
        t <- term
        setState ctx
        return $ TmAbs "_" t

-- tmApp := appTerm tmVar
tmApp :: Parser (Term -> Term -> Term)
tmApp = return TmApp

-- tmVar := lcid
tmVar :: Parser Term
tmVar = do
    name <- lcid
    ctx <- getState
    index <- name2Index' ctx name
    return $ TmVar index (lengthContext ctx)
  where
    name2Index' ctx name = case name2Index ctx name of
        Left s -> unexpected s
        Right i -> return i

-- surrounded := '(' term ')'
surrounded :: Parser Term
surrounded = between (symbol "(") (symbol ")") term
