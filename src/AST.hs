{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module AST where

import Prelude hiding (Ordering(..))

import Data.String.Interpolate ( i )
import Flow

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

printAST :: ICFP Var -> IO ()
printAST = prettyAST .> putStrLn

prettyAST :: ICFP Var -> String
prettyAST = \case
  V var -> showVar var
  L var body -> [i|\\#{showVar var} -> #{prettyAST body}|]
  If b x y -> [i|if #{prettyAST b} then #{prettyAST x} else #{prettyAST y}|]
  T -> "True"
  F -> "False"
  S str -> show str
  I x -> show x
  U uop term -> [i|#{prettyUOP uop} (#{prettyAST term})|]
  B bop x y -> [i|(#{prettyAST x})#{prettyBOP bop}(#{prettyAST y})|]
  where
    showVar var = "x_" ++ show var

prettyUOP :: UOp -> String
prettyUOP = \case
  Neg -> "negate"
  Not -> "not"
  S2I -> "str2int"
  I2S -> "int2str"

prettyBOP :: BOp -> String
prettyBOP = \case
  Add -> " + "
  Sub -> " - "
  Mul -> " * "
  Div -> " `div` "
  Mod -> " `mod` "
  LT -> " < "
  GT -> " > "
  EQ -> " == "
  Or -> " || "
  And -> " && "
  Cat -> " ++ "
  Take -> " `take` "
  Drop -> " `drop` "
  App -> " "
