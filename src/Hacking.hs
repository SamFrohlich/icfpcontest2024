module Hacking where

import Data.List.Split
import qualified Data.Map as M

import NumberMap
import StringMap

import Debug.Trace

program :: String
program = "? B= B$ B$ B$ B$ L$ L$ L$ L# v$ I\" I# I$ I% I$ ? B= B$ L$ v$ I+ I+ ? B= BD I$ S4%34 S4 ? B= BT I$ S4%34 S4%3 ? B= B. S4% S34 S4%34 ? U! B& T F ? B& T T ? U! B| F F ? B| F T ? B< U- I$ U- I# ? B> I$ I# ? B= U- I\" B% U- I$ I# ? B= I\" B% I( I$ ? B= U- I\" B/ U- I$ I# ? B= I# B/ I( I$ ? B= I' B* I# I$ ? B= I$ B+ I\" I# ? B= U$ I4%34 S4%34 ? B= U# S4%34 I4%34 ? U! F ? B= U- I$ B- I# I& ? B= I$ B- I& I# ? B= S4%34 S4%34 ? B= F F ? B= I$ I$ ? T B. B. SM%,&k#(%#+}IEj}3%.$}z3/,6%},!.'5!'%y4%34} U$ B+ I# B* I$> I1~s:U@ Sz}4/}#,!)-}0/).43}&/2})4 S)&})3}./4}#/22%#4 S\").!29}q})3}./4}#/22%#4 S\").!29}q})3}./4}#/22%#4 S\").!29}q})3}./4}#/22%#4 S\").!29}k})3}./4}#/22%#4 S5.!29}k})3}./4}#/22%#4 S5.!29}_})3}./4}#/22%#4 S5.!29}a})3}./4}#/22%#4 S5.!29}b})3}./4}#/22%#4 S\").!29}i})3}./4}#/22%#4 S\").!29}h})3}./4}#/22%#4 S\").!29}m})3}./4}#/22%#4 S\").!29}m})3}./4}#/22%#4 S\").!29}c})3}./4}#/22%#4 S\").!29}c})3}./4}#/22%#4 S\").!29}r})3}./4}#/22%#4 S\").!29}p})3}./4}#/22%#4 S\").!29}{})3}./4}#/22%#4 S\").!29}{})3}./4}#/22%#4 S\").!29}d})3}./4}#/22%#4 S\").!29}d})3}./4}#/22%#4 S\").!29}l})3}./4}#/22%#4 S\").!29}N})3}./4}#/22%#4 S\").!29}>})3}./4}#/22%#4 S!00,)#!4)/.})3}./4}#/22%#4 S!00,)#!4)/.})3}./4}#/22%#4"

data ICFP =
  T | F
  | I Int
  | S String
  | UNeg | UNot | US2I | UI2S
  | BAdd | BSub | BMul | BDiv | BMod
  | BLT | BGT | BEQ
  | BOr | BAnd
  | BCat | BTake | BDrop
  | BApp
  | If
  | Lam Int -- variable number
  | Var Int -- ditto
  deriving (Show, Eq)

toICFP :: String -> [ICFP]
toICFP program = fmap toICFP' (splitOn " " program)
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

data Value = ZZ Int | SS String | BB Bool | LL Int [ICFP]
  deriving (Show, Eq)

eval :: M.Map Int Value -> [ICFP] -> (Value, [ICFP])
eval mem (T : rest) = (BB True, rest)
eval mem (F : rest) = (BB False, rest)
eval mem (I z : rest) = (ZZ z, rest)
eval mem (S s : rest) = (SS s, rest)
eval mem (UNeg : rest) =
  let (ZZ z, rest') = eval mem rest
  in (ZZ (negate z), rest')
eval mem (UNot : rest) =
  let (BB b, rest') = eval mem rest
  in (BB (not b), rest')
eval mem (US2I : rest) =
  let (SS s, rest') = eval mem rest
  in (ZZ (fromBase94 s), rest')
eval mem (UI2S : rest) =
  let (ZZ z, rest') = eval mem rest
  in (SS (toBase94 z), rest')
eval mem (BAdd : rest) =
  let (ZZ x, rest') = eval mem rest
      (ZZ y, rest'') = eval mem rest'
  in (ZZ (x + y), rest'')
eval mem (BSub : rest) =
  let (ZZ x, rest') = eval mem rest
      (ZZ y, rest'') = eval mem rest'
  in (ZZ (x - y), rest'')
eval mem (BMul : rest) =
  let (ZZ x, rest') = eval mem rest
      (ZZ y, rest'') = eval mem rest'
  in (ZZ (x * y), rest'')
eval mem (BDiv : rest) =
  let (ZZ x, rest') = eval mem rest
      (ZZ y, rest'') = eval mem rest'
  in (ZZ (x `div` y), rest'')
eval mem (BMod : rest) =
  let (ZZ x, rest') = eval mem rest
      (ZZ y, rest'') = eval mem rest'
  in (ZZ (x `mod` y), rest'')
eval mem (BLT : rest) =
  let (ZZ x, rest') = eval mem rest
      (ZZ y, rest'') = eval mem rest'
  in (BB (x < y), rest'')
eval mem (BGT : rest) =
  let (ZZ x, rest') = eval mem rest
      (ZZ y, rest'') = eval mem rest'
  in (BB (x > y), rest'')
eval mem (BEQ : rest) =
  let (x, rest') = eval mem rest
      (y, rest'') = eval mem rest'
  in (BB (x == y), rest'')
eval mem (BOr : rest) =
  let (BB a, rest') = eval mem rest
      (BB b, rest'') = eval mem rest'
  in (BB (a || b), rest'')
eval mem (BAnd : rest) =
  let (BB a, rest') = eval mem rest
      (BB b, rest'') = eval mem rest'
  in (BB (a && b), rest'')
eval mem (BCat : rest) =
  let (SS s, rest') = eval mem rest
      (SS s', rest'') = eval mem rest'
  in (SS (s ++ s'), rest'')
eval mem (BTake : rest) =
  let (ZZ z, rest') = eval mem rest
      (SS s, rest'') = eval mem rest'
  in (SS (take z s), rest'')
eval mem (BDrop : rest) =
  let (ZZ z, rest') = eval mem rest
      (SS s, rest'') = eval mem rest'
  in (SS (drop z s), rest'')
eval mem (BApp : rest) =
  let (LL v lam, rest') = eval mem rest
      (x, rest'') = eval mem rest'
      mem' = M.alter (const $ Just x) v mem
  in trace ("app" ++ show v ++ ":" ++ show lam ++ ":" ++ show mem') $ eval mem' lam
eval mem (If : rest) =
  let (BB b, rest') = eval mem rest
      (suc, rest'') = eval mem rest'
      (fal, rest''') = eval mem rest'
  in (if b then suc else fal, rest''')
eval mem (Lam v : rest) = trace ("lam" ++ show v ++ ":" ++ show mem) (LL v (lam), rest')
  where
    (lam, rest') = readLambda rest 1
    readLambda rest'' 0 = ([], rest'')
    readLambda (token : rest'') n =
      case arity token of
        Arity 0 ->
          let (lam, rest''') = readLambda rest'' (n-1)
          in (token : lam, rest''')
        Arity 1 ->
          let (lam, rest''') = readLambda rest'' n
          in (token : lam, rest''')
        Arity 2 ->
          let (lam, rest''') = readLambda rest'' (n+1)
          in (token : lam, rest''')
        Arity 3 ->
          let (lam, rest''') = readLambda rest'' (n+2)
          in (token : lam, rest''')
eval mem (Var v : rest) = trace ("var" ++ show v ++ ":" ++ show rest ++ ":" ++ show mem) (mem M.! v, rest)

newtype Arity = Arity Int
arity :: ICFP -> Arity
arity (I _) = Arity 0
arity (S _) = Arity 0
arity (Lam _) = Arity 1
arity (Var _) = Arity 0
arity token
  | token `elem` [T, F] = Arity 0
  | token `elem` [UNeg, UNot, US2I, UI2S] = Arity 1
  | token `elem` [BAdd, BSub, BMul, BDiv, BMod, BLT, BGT, BEQ, BOr, BAnd, BCat, BTake, BDrop, BApp] = Arity 2
  | token `elem` [If] = Arity 3
