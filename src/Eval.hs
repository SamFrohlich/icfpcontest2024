{-# LANGUAGE GADTs #-}

module Eval where

import AST
import NumberMap

import Prelude hiding (Ordering(..))

import Data.HList

eval :: ICFP a -> a
eval = eval' HNil
  where
    eval' :: HList mem -> ICFP a -> a
    eval' _ T = True
    eval' _ F = False
    eval' _ (I z) = z
    eval' _ (S s) = s
    eval' mem (U Neg z) = negate (eval' mem z)
    eval' mem (U Not b) = not (eval' mem b)
    eval' mem (U STI s) = fromBase94 (eval' mem s)
    eval' mem (U ITS z) = toBase94 (eval' mem z)
    eval' mem (B Add x y) = eval' mem x + eval' mem y
    eval' mem (B Sub x y) = eval' mem x - eval' mem y
    eval' mem (B Mul x y) = eval' mem x * eval' mem y
    eval' mem (B Div x y) = eval' mem x `div` eval' mem y
    eval' mem (B Mod x y) = eval' mem x `mod` eval' mem y
    eval' mem (B LT x y) = (eval' mem x) < (eval' mem y) -- NOTE assuming strict
    eval' mem (B GT x y) = eval' mem x > eval' mem y
    eval' mem (B EQ x y) = eval' mem x == eval' mem y
    eval' mem (B Or x y) = eval' mem x || eval' mem y
    eval' mem (B And x y) = eval' mem x && eval' mem y
    eval' mem (B Cat x y) = eval' mem x ++ eval' mem y
    eval' mem (B Take x y) = take (eval' mem x) (eval' mem y)
    eval' mem (B Drop x y) = drop (eval' mem x) (eval' mem y)
    eval' mem (B App x y) = (eval' mem x) (eval' mem y)
    eval' mem (Q b t f) = if (eval' mem b) then eval' mem t else eval' mem f
    eval' mem (L v body arg) = undefined
