{-# LANGUAGE LambdaCase #-}
module Hacking where

import Data.List.Split
import qualified Data.Map as M

import NumberMap
import StringMap

import AST (UOp(..), BOp(..), astToStr)
import AST qualified
import Data.List (foldl')
import Flow

program :: String
program = "? B= B$ B$ B$ B$ L$ L$ L$ L# v$ I\" I# I$ I% I$ ? B= B$ L$ v$ I+ I+ ? B= BD I$ S4%34 S4 ? B= BT I$ S4%34 S4%3 ? B= B. S4% S34 S4%34 ? U! B& T F ? B& T T ? U! B| F F ? B| F T ? B< U- I$ U- I# ? B> I$ I# ? B= U- I\" B% U- I$ I# ? B= I\" B% I( I$ ? B= U- I\" B/ U- I$ I# ? B= I# B/ I( I$ ? B= I' B* I# I$ ? B= I$ B+ I\" I# ? B= U$ I4%34 S4%34 ? B= U# S4%34 I4%34 ? U! F ? B= U- I$ B- I# I& ? B= I$ B- I& I# ? B= S4%34 S4%34 ? B= F F ? B= I$ I$ ? T B. B. SM%,&k#(%#+}IEj}3%.$}z3/,6%},!.'5!'%y4%34} U$ B+ I# B* I$> I1~s:U@ Sz}4/}#,!)-}0/).43}&/2})4 S)&})3}./4}#/22%#4 S\").!29}q})3}./4}#/22%#4 S\").!29}q})3}./4}#/22%#4 S\").!29}q})3}./4}#/22%#4 S\").!29}k})3}./4}#/22%#4 S5.!29}k})3}./4}#/22%#4 S5.!29}_})3}./4}#/22%#4 S5.!29}a})3}./4}#/22%#4 S5.!29}b})3}./4}#/22%#4 S\").!29}i})3}./4}#/22%#4 S\").!29}h})3}./4}#/22%#4 S\").!29}m})3}./4}#/22%#4 S\").!29}m})3}./4}#/22%#4 S\").!29}c})3}./4}#/22%#4 S\").!29}c})3}./4}#/22%#4 S\").!29}r})3}./4}#/22%#4 S\").!29}p})3}./4}#/22%#4 S\").!29}{})3}./4}#/22%#4 S\").!29}{})3}./4}#/22%#4 S\").!29}d})3}./4}#/22%#4 S\").!29}d})3}./4}#/22%#4 S\").!29}l})3}./4}#/22%#4 S\").!29}N})3}./4}#/22%#4 S\").!29}>})3}./4}#/22%#4 S!00,)#!4)/.})3}./4}#/22%#4 S!00,)#!4)/.})3}./4}#/22%#4"

lambdaEg :: String
lambdaEg = "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK"

ifEg :: AST.ICFP Integer
ifEg = strToAST "? B> I# I$ S9%3 S./"


strToAST :: String -> AST.ICFP Integer
strToAST = toICFP .> toAST

-- >>> strToAST lambdaEg
-- B App (B App (L 2 (L 3 (V 2))) (B Cat (S "Hello") (S " World!"))) (I 42)

rountTripAST :: String -> String
rountTripAST str = str |> strToAST |> astToStr

data ICFP =
  T | F
  | I Integer
  | S String
  | UNeg | UNot | US2I | UI2S
  | BAdd | BSub | BMul | BDiv | BMod
  | BLT | BGT | BEQ
  | BOr | BAnd
  | BCat | BTake | BDrop
  | BApp
  | If
  | Lam Integer -- variable number
  | Var Integer -- ditto
  deriving (Show, Eq)

toICFP :: String -> [ICFP]
toICFP prog = fmap toICFP' (splitOn " " prog)
  where
    toICFP' "T" = T
    toICFP' "F" = F
    toICFP' ('I' : rest) =
      let x = fromBase94 rest
      in I x
    toICFP' ('S' : rest) = S (fromIcfpString rest) -- should they be translated???
    toICFP' "U-" = UNeg
    toICFP' "U!" = UNot
    toICFP' "U#" = US2I
    toICFP' "U$" = UI2S
    toICFP' "B+" = BAdd
    toICFP' "B-" = BSub
    toICFP' "B*" = BMul
    toICFP' "B/" = BDiv
    toICFP' "B%" = BMod
    toICFP' "B<" = BLT
    toICFP' "B>" = BGT
    toICFP' "B=" = BEQ
    toICFP' "B|" = BOr
    toICFP' "B&" = BAnd
    toICFP' "B." = BCat
    toICFP' "BT" = BTake
    toICFP' "BD" = BDrop
    toICFP' "B$" = BApp
    toICFP' "?" = If
    toICFP' ('L' : name) = Lam (fromBase94 name)
    toICFP' ('v' : name) = Var (fromBase94 name)
    toICFP' _ = error "Invalid token in string!"

toAST :: [ICFP] -> AST.ICFP Integer
toAST = reverse .> foldl' f [] .> head
  where
    f :: [AST.ICFP Integer] -> ICFP -> [AST.ICFP Integer]
    f stack tok = stack |> case tok of
      -- 0-arity
      T -> arity0 (AST.T)
      F -> arity0 (AST.F)
      I x -> arity0 (AST.I x)
      S str -> arity0 (AST.S str)
      Var i -> arity0 (AST.V i)
      -- 1-arity
      Lam i -> arity1 (AST.L i)
      UNeg  -> arity1 (AST.U Neg)
      UNot  -> arity1 (AST.U Not)
      US2I  -> arity1 (AST.U S2I)
      UI2S  -> arity1 (AST.U I2S)

      -- 2-arity
      BAdd -> arity2 (AST.B Add)
      BSub -> arity2 (AST.B Sub )
      BMul -> arity2 (AST.B Mul )
      BDiv -> arity2 (AST.B Div )
      BMod -> arity2 (AST.B Mod )
      BLT  -> arity2 (AST.B AST.LT)
      BGT  -> arity2 (AST.B AST.GT)
      BEQ  -> arity2 (AST.B AST.EQ)
      BOr  -> arity2 (AST.B Or  )
      BAnd -> arity2 (AST.B And )
      BCat -> arity2 (AST.B Cat )
      BTake-> arity2 (AST.B Take)
      BDrop-> arity2 (AST.B Drop)
      BApp -> arity2 (AST.B App )

      -- 3-arity
      If -> arity3 AST.If

arity0 :: AST.ICFP Integer -> [AST.ICFP Integer] -> [AST.ICFP Integer]
arity0 op stack = op : stack

arity1 :: (AST.ICFP Integer -> AST.ICFP Integer) -> [AST.ICFP Integer] -> [AST.ICFP Integer]
arity1 op = \case
  operand : stack -> op operand : stack
  [] -> error "arity1 operator expected an operand on the stack, but it was empty!"

arity2 :: (AST.ICFP Integer -> AST.ICFP Integer -> AST.ICFP Integer) -> [AST.ICFP Integer] -> [AST.ICFP Integer]
arity2 op = \case
  x : y : stack -> op x y : stack
  _ : [] -> error "arity2 operator expected 2 operands, but 1 on stack!"
  [] -> error "arity2 operator expected 2 operands, but 0 on stack!"

arity3 :: (AST.ICFP Integer -> AST.ICFP Integer -> AST.ICFP Integer -> AST.ICFP Integer)
       -> [AST.ICFP Integer] -> [AST.ICFP Integer]
arity3 op = \case
  x : y : z : stack -> op x y z : stack
  _ : _ : [] -> error "arity3 operator expected 3 operands, but 2 on stack!"
  _ : [] -> error "arity3 operator expected 3 operands, but 1 on stack!"
  [] -> error "arity2 operator expected 2 operands, but 0 on stack!"


-- data Value = ZZ Integer | SS String | BB Bool | LL Integer [ICFP]
--   deriving (Show, Eq)

-- eval :: M.Map Integer Value -> [ICFP] -> (Value, [ICFP])
-- eval mem (T : rest) = (BB True, rest)
-- eval mem (F : rest) = (BB False, rest)
-- eval mem (I z : rest) = (ZZ z, rest)
-- eval mem (S s : rest) = (SS s, rest)
-- eval mem (UNeg : rest) =
--   let (ZZ z, rest') = eval mem rest
--   in (ZZ (negate z), rest')
-- eval mem (UNot : rest) =
--   let (BB b, rest') = eval mem rest
--   in (BB (not b), rest')
-- eval mem (US2I : rest) =
--   let (SS s, rest') = eval mem rest
--   in (ZZ (fromBase94 s), rest')
-- eval mem (UI2S : rest) =
--   let (ZZ z, rest') = eval mem rest
--   in (SS (toBase94 z), rest')
-- eval mem (BAdd : rest) =
--   let (ZZ x, rest') = eval mem rest
--       (ZZ y, rest'') = eval mem rest'
--   in (ZZ (x + y), rest'')
-- eval mem (BSub : rest) =
--   let (ZZ x, rest') = eval mem rest
--       (ZZ y, rest'') = eval mem rest'
--   in (ZZ (x - y), rest'')
-- eval mem (BMul : rest) =
--   let (ZZ x, rest') = eval mem rest
--       (ZZ y, rest'') = eval mem rest'
--   in (ZZ (x * y), rest'')
-- eval mem (BDiv : rest) =
--   let (ZZ x, rest') = eval mem rest
--       (ZZ y, rest'') = eval mem rest'
--   in (ZZ (x `div` y), rest'')
-- eval mem (BMod : rest) =
--   let (ZZ x, rest') = eval mem rest
--       (ZZ y, rest'') = eval mem rest'
--   in (ZZ (x `mod` y), rest'')
-- eval mem (BLT : rest) =
--   let (ZZ x, rest') = eval mem rest
--       (ZZ y, rest'') = eval mem rest'
--   in (BB (x < y), rest'')
-- eval mem (BGT : rest) =
--   let (ZZ x, rest') = eval mem rest
--       (ZZ y, rest'') = eval mem rest'
--   in (BB (x > y), rest'')
-- eval mem (BEQ : rest) =
--   let (x, rest') = eval mem rest
--       (y, rest'') = eval mem rest'
--   in (BB (x == y), rest'')
-- eval mem (BOr : rest) =
--   let (BB a, rest') = eval mem rest
--       (BB b, rest'') = eval mem rest'
--   in (BB (a || b), rest'')
-- eval mem (BAnd : rest) =
--   let (BB a, rest') = eval mem rest
--       (BB b, rest'') = eval mem rest'
--   in (BB (a && b), rest'')
-- eval mem (BCat : rest) =
--   let (SS s, rest') = eval mem rest
--       (SS s', rest'') = eval mem rest'
--   in (SS (s ++ s'), rest'')
-- eval mem (BTake : rest) =
--   let (ZZ z, rest') = eval mem rest
--       (SS s, rest'') = eval mem rest'
--   in (SS (take z s), rest'')
-- eval mem (BDrop : rest) =
--   let (ZZ z, rest') = eval mem rest
--       (SS s, rest'') = eval mem rest'
--   in (SS (drop z s), rest'')
-- eval mem (BApp : rest) =
--   let (LL v lam, rest') = eval mem rest
--       (x, rest'') = eval mem rest'
--       mem' = M.alter (const $ Just x) v mem
--   in trace ("app" ++ show v ++ ":" ++ show lam ++ ":" ++ show mem') $ eval mem' lam
-- eval mem (If : rest) =
--   let (BB b, rest') = eval mem rest
--       (suc, rest'') = eval mem rest'
--       (fal, rest''') = eval mem rest'
--   in (if b then suc else fal, rest''')
-- eval mem (Lam v : rest) = trace ("lam" ++ show v ++ ":" ++ show mem) (LL v (lam), rest')
--   where
--     (lam, rest') = readLambda rest 1
--     readLambda rest'' 0 = ([], rest'')
--     readLambda (token : rest'') n =
--       case arity token of
--         Arity 0 ->
--           let (lam, rest''') = readLambda rest'' (n-1)
--           in (token : lam, rest''')
--         Arity 1 ->
--           let (lam, rest''') = readLambda rest'' n
--           in (token : lam, rest''')
--         Arity 2 ->
--           let (lam, rest''') = readLambda rest'' (n+1)
--           in (token : lam, rest''')
--         Arity 3 ->
--           let (lam, rest''') = readLambda rest'' (n+2)
--           in (token : lam, rest''')
-- eval mem (Var v : rest) = trace ("var" ++ show v ++ ":" ++ show rest ++ ":" ++ show mem) (mem M.! v, rest)

-- newtype Arity = Arity Integer
-- arity :: ICFP -> Arity
-- arity (I _) = Arity 0
-- arity (S _) = Arity 0
-- arity (Lam _) = Arity 1
-- arity (Var _) = Arity 0
-- arity token
--   | token `elem` [T, F] = Arity 0
--   | token `elem` [UNeg, UNot, US2I, UI2S] = Arity 1
--   | token `elem` [BAdd, BSub, BMul, BDiv, BMod, BLT, BGT, BEQ, BOr, BAnd, BCat, BTake, BDrop, BApp] = Arity 2
--   | token `elem` [If] = Arity 3
