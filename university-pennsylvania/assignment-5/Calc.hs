module Calc where

import Data.Maybe

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit num) = num
eval (Add expOne expTwo) = (eval expOne) + (eval expTwo)
eval (Mul expOne expTwo) = (eval expOne) * (eval expTwo)

evalStr :: String -> Maybe Integer
evalStr str = do
    let parseResult = parseExp Lit Add Mul str
    case parseResult of
        Nothing -> Nothing
        Just res -> Just (eval res)

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit num = (Lit num)
    add expOne expTwo = (Add expOne expTwo)
    mul expOne expTwo = (Mul expOne expTwo)

reify :: ExprT -> ExprT
reify = id
