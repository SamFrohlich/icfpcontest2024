{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module AST
  (UOp(..), BOp(..), Var, ICFP(..))
where

import Prelude hiding (Ordering(..))

data UOp a b where
  Neg :: UOp Int Int
  Not :: UOp Bool Bool
  STI :: UOp String Int
  ITS :: UOp Int String

deriving instance Show (UOp a b)
deriving instance Eq (UOp a b)

data BOp a b c where
  Add, Sub, Mul, Div, Mod :: BOp Int Int Int
  LT, GT :: BOp Int Int Bool
  EQ :: Eq a => BOp a a Bool
  Or, And :: BOp Bool Bool Bool
  Cat :: BOp String String String
  Take, Drop :: BOp Int String String
  App :: BOp (a -> b) a b

deriving instance Show (BOp a b c)
deriving instance Eq (BOp a b c)

type Var = String

data ICFP a where
  T, F :: ICFP Bool
  I :: Int -> ICFP Int
  S :: String -> ICFP String
  U :: UOp a b -> ICFP a -> ICFP b
  B :: BOp a b c -> ICFP a -> ICFP b -> ICFP c
  Q :: ICFP Bool -> ICFP a -> ICFP a -> ICFP a -- potentially Either a b
  L :: Var -> ICFP b -> ICFP a -> ICFP (a -> b)
  V :: Var -> ICFP a

deriving instance Show (ICFP a)

example1 :: ICFP Bool
example1 = U Not T
