{-# LANGUAGE OverloadedStrings #-}

module Core where

import Control.Applicative

--
-- Syntax 
--

data Command
    = Eval Term
    deriving (Show)

data Term
    = TmTrue
    | TmFalse
    | TmIf Term Term Term
    | TmZero
    | TmSucc Term
    | TmPred Term
    | TmIsZero Term

instance Show Term where
    -- show :: Term -> String
    show TmTrue          = "true"
    show TmFalse         = "false"
    show (TmIf t1 t2 t3) = "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3
    show TmZero          = "0"
    show (TmSucc t)      = showAsNum t (1 :: Integer)
      where
        showAsNum TmZero      num = show num
        showAsNum (TmSucc t') num = showAsNum t' (num + 1)
        showAsNum _           _   = "succ (" ++ show t ++ ")"
    show (TmPred t)      = "pred (" ++ show t ++ ")"
    show (TmIsZero t)    = "iszero " ++ show t

--
-- Evaluation
--

isNumericVal :: Term -> Bool
isNumericVal TmZero = True
isNumericVal (TmSucc t1) = isNumericVal t1
isNumericVal _ = False

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t | isNumericVal t = True
isVal _ = False

eval :: Term -> Term
eval t = case eval1 t of
             Nothing -> t
             Just t' -> eval t'

eval1 :: Term -> Maybe Term
eval1 (TmIf TmTrue  t2 _)  = pure t2
eval1 (TmIf TmFalse _  t3) = pure t3
eval1 (TmIf t1      t2 t3) = (flip . flip TmIf) t2 t3 <$> eval1 t1
eval1 (TmSucc t1)          = TmSucc <$> eval1 t1
eval1 (TmPred TmZero)      = pure TmZero
eval1 (TmPred (TmSucc t1))
    | isNumericVal t1      = pure t1
eval1 (TmPred t1)          = TmPred <$> eval1 t1
eval1 (TmIsZero TmZero)    = pure TmTrue
eval1 (TmIsZero (TmSucc t1))
    | isNumericVal t1      = pure TmFalse
eval1 (TmIsZero t1)        = TmIsZero <$> eval1 t1
eval1 _                    = Nothing
