{-# LANGUAGE GADTs #-}

module AST where

import Prelude hiding (Ordering)

data UOp a b where
  Neg :: UOp Int Int
  Not :: UOp Bool Bool
  STI :: UOp String Int
  ITS :: UOp Int String

data BOp a b c where
  Add, Sub, Mul, Div, Mod :: BOp Int Int Int
  LT, GT :: BOp Int Int Bool
  EQ :: BOp a a Bool
  Or, And :: BOp Bool Bool Bool
  Cat :: BOp String String String
  Take, Drop :: BOp Int String String
  App :: BOp (a -> b) a b

type Var = Int

data ICFP a where
  T, F :: ICFP Bool
  I :: Int -> ICFP Int
  S :: String -> ICFP String
  U :: UOp a b -> ICFP a -> ICFP b
  B :: BOp a b c -> ICFP a -> ICFP b -> ICFP c
  Q :: ICFP Bool -> ICFP a -> ICFP a -> ICFP a -- potentially Either a b
  L :: Var -> ICFP b -> ICFP a -> ICFP (a -> b)
