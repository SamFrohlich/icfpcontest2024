{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module AST where

import Prelude hiding (Ordering(..))

data UOp
  = Neg
  | Not
  | S2I
  | I2S
  deriving (Eq, Ord, Show)

data BOp
  = Add | Sub | Mul | Div | Mod
  | LT | GT
  | EQ
  | Or | And
  | Cat
  | Take | Drop
  | App
  deriving (Eq, Ord, Show)

type Var = Int

data ICFP var
  = T | F
  | I Int
  | S String
  | U UOp (ICFP var)
  | B BOp (ICFP var) (ICFP var)
  | If (ICFP var) (ICFP var) (ICFP var)
  | L var (ICFP var)
  | V var
  deriving (Eq, Ord, Show)

example1 :: ICFP Var
example1 = U Not T
