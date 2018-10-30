module Calc where

import ExprT

eval :: ExprT -> Integer
eval (Lit num) = num
eval (Add expOne expTwo) = (eval expOne) + (eval expTwo)
eval (Mul expOne expTwo) = (eval expOne) * (eval expTwo)
