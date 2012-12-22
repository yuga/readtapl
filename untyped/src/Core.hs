{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Core where

import Control.Applicative
import Control.Monad.Instances  ()
import Data.Monoid
import MinPrint
import Prelude                   hiding (catch)

--import Debug.Trace

--
-- Syntax 
--

data Term
    = TmVar Int Int         -- ^ TmVar holds de Bruijn index and the length of the context
    | TmAbs String Term     -- ^ TmAbs holds the bound variable names
    | TmApp Term Term       -- ^ TmApp holds two lambda terms called an applicaiton
    deriving (Show)

data Binding
    = NameBind
    deriving (Show)

newtype Context
    = Context { bs :: [(String, Binding)] }

data Command
    = Eval Term
    | Bind String Binding
    deriving (Show)

newtype MIO a 
    = MIO { unMIO :: IO a }

instance Functor MIO where
    fmap f m = MIO $ f <$> unMIO m 

instance Applicative MIO where
    pure = return
    f <*> m = MIO $ unMIO f <*> unMIO m

instance Monad MIO where
    return a = MIO $ return a
    m >>= f = MIO $ unMIO m >>= unMIO . f

instance (Monoid a) => Monoid (MIO a) where
    mempty = pure mempty
    mappend = (*>)

--
-- Printing
--

printTerm :: Context -> Term -> IO ()
printTerm ctx t = unMIO $ printDoc (MIO . putStr) $ execMinPrint $ printTmAbs ctx t

printTmAbs :: Context -> Term -> MinPrint String (Item String)
printTmAbs ctx (TmAbs  x t2) = do
    let (ctx', x') = pickFreshName ctx x
    nest 2 $ do
        text "lambda "
        text x'
        text ". "
        printTmAbs ctx' t2
printTmAbs ctx t = printTmApp ctx t

printTmApp :: Context -> Term -> MinPrint String (Item String)
printTmApp ctx (TmApp t1 t2) = do
    nest 0 $ do
        printTmApp ctx t1
        text " "
        printTmVar ctx t2
printTmApp ctx t = printTmVar ctx t

printTmVar :: Context -> Term -> MinPrint String (Item String)
printTmVar ctx (TmVar x n) = do
    if lengthContext ctx == n then
        text $ case index2Name ctx x of { Left s -> s; Right s -> s }
     else
        text $ "[bad index: " ++ show x ++ "/" ++ show n
             ++ " in {"
             ++ (foldl (\s (x',_) -> s ++ " " ++ x') "" (bs ctx))
             ++ "}]"
printTmVar ctx t = do
    text "("
    printTmAbs ctx t
    text ")"

printBinding :: Binding -> IO ()
printBinding NameBind = return ()

--
-- Context Management
--

emptyContext :: Context
emptyContext = Context []

lengthContext :: Context -> Int
lengthContext = length . bs

appendBinding :: Context -> String -> Binding -> Context
appendBinding ctx s bind = ctx { bs = (s, bind) : (bs ctx)  }

appendNameBind :: Context -> String -> Context
appendNameBind ctx s = appendBinding ctx s NameBind

existsNameBind :: Context -> String -> Bool
existsNameBind ctx s = go $ bs ctx
  where
    go []                     = False
    go ((x,_):xs) | x == s    = True
                  | otherwise = go xs

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx s | not (existsNameBind ctx s) = (appendNameBind ctx s, s)
                    | otherwise                  = pickFreshName ctx (s ++ "'")

index2Name :: Context -> Int -> Either String String
index2Name ctx i =
    if lengthContext ctx > i then
        Right x
    else
        Left $ "Variable lookup failure: offset = " ++ (show i)
            ++ ", ctx size = " ++ (show $ lengthContext ctx)
  where
    (x,_) = bs ctx !! i

name2Index :: Context -> String -> Either String Int
name2Index ctx s = go $ bs ctx
  where
    go []                     = Left $ "Identifier " ++ s ++ " is unbound"
    go ((x,_):xs) | x == s    = Right 0
                  | otherwise = go xs >>= \i -> Right (i + 1)

getBinding :: Context -> Int -> Binding
getBinding ctx i =
    if lengthContext ctx > i then
        b
    else
        error $ "Variable lookup failure: offset = " ++ (show i)
             ++ ", ctx size = " ++ (show $ lengthContext ctx)
  where
    (_,b) = bs ctx !! i

--
-- Shifting & Substitution
--

tmmap :: (Int -> Int -> Int -> Term) -> Int -> Term -> Term
tmmap onvar = go
  where
    go c (TmVar x  n)  = onvar c x n
    go c (TmAbs x  t1) = TmAbs x (go (c+1) t1)
    go c (TmApp t1 t2) = TmApp (go c t1) (go c t2)

-- termShiftAbove d c t = tmmap onvar c t = shiftUp_c^d(t)
termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d c t = tmmap onvar c t
  where
    onvar c' x n | x >= c'   = TmVar (x+d) (n+d)
                 | otherwise = TmVar x (n+d)

termShift :: Int -> Term -> Term
termShift d t = termShiftAbove d 0 t

-- termSubst j s t = tmmap onvar 0 t = [j -> s]t
termSubst :: Int -> Term -> Term -> Term
termSubst j s t = tmmap onvar 0 t
  where
    onvar c' x n | x == j+c' = termShift c' s
                 | otherwise = TmVar x n

-- termSubstTop s t = shiftUp_0^(-1)([0 -> shiftUp_0^1(s)]t)
termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

--
-- Evaluation
--

eval :: Term -> Term
eval t = case eval1 t of
    Left  _  -> t
    Right t' -> t'

eval1 :: Term -> Either String Term
eval1 (TmApp (TmAbs _ t12) v2) | isVal v2 = return $ termSubstTop v2 t12
eval1 (TmApp v1 t2)            | isVal v1 = TmApp v1 <$> eval1 t2
eval1 (TmApp t1 t2)                       = eval1 t1 >>= \t1' -> return $ TmApp t1' t2
eval1 _                                   = Left "no rules to apply"

isVal :: Term -> Bool
isVal (TmAbs _ _) = True
isVal _ = False

